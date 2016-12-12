-module(ucd).
-export([ files/0
        , file/1
        , fold_lines/2
        , fold_lines/3
        , fold_lines/4
        , codepoint/1
        , codepoint_range/1
        , codepoint_or_range/1
        , codepoints/1
        , compact/1
        , sort_by_codepoints/1
        , to_integer/1
        ]).

-include_lib("stdlib/include/zip.hrl").

-define(UCD_ZIP,"UCD.zip").


files() ->
    lists:flatmap(fun list_files/1, data_files()).


file(FileName) ->
    ZipFileName = zip_file(FileName),
    case zip:unzip(ZipFileName, [{file_list, [FileName]}, memory]) of
        {ok, [{_, Data}]} -> Data;
        _                 -> error(badarg)
    end.


fold_lines(Fun, FileName) ->
    Acc1 = fold_lines(fun (L,Acc) -> [Fun(L) | Acc] end, FileName, []),
    lists:reverse(Acc1).


fold_lines(Fun, FileName, Acc) ->
    fold_lines(Fun, FileName, Acc, [strip_comment, fields]).


fold_lines(Fun, FileName, Acc, Opts) ->
    Data = file(FileName),
    StripComment = proplists:get_bool(strip_comment, Opts),
    SplitFields = proplists:get_bool(fields, Opts),
    FoldFun = fold_lines_fun(Fun, StripComment, SplitFields),
    fold_lines_1(FoldFun, Acc, binary:split(Data, <<"\n">>)).


fold_lines_fun(Fun, true, true) ->
    MP = fields_re(),
    fun (Line, Acc) -> Fun(fields(MP, strip_comment(Line)), Acc) end;

fold_lines_fun(Fun, true, false) ->
    fun (Line, Acc) -> Fun(strip_comment(Line), Acc) end;

fold_lines_fun(Fun, false, true) ->
    MP = fields_re(),
    fun (Line, Acc) -> Fun(fields(MP, Line), Acc) end;

fold_lines_fun(Fun, false, false) ->
    Fun.


fold_lines_1(_, Acc, [<<>>]) ->
    Acc;

fold_lines_1(Fun, Acc0, [Bin, Rest]) ->
    Acc1 = case string:strip(binary_to_list(Bin), right, $\n) of
               ""       -> Acc0;
               "#" ++ _ -> Acc0;
               Data     -> Fun(Data, Acc0)
           end,
    fold_lines_1(Fun, Acc1, binary:split(Rest, <<"\n">>)).


data_dir() ->
    code:priv_dir(unicodedata).


data_files() ->
    DataDir = data_dir(),
    filelib:wildcard(filename:join(DataDir, "*.zip")).


list_files(ZipFile) ->
    {ok, Files} = zip:list_dir(ZipFile),
    [Name || #zip_file{name=Name} <- Files].


zip_file(FileName) ->
    ZipFile = case string:tokens(FileName, [$_]) of
                  [_]          -> ?UCD_ZIP;
                  [Prefix | _] -> Prefix ++ ".zip"
              end,
    filename:join(data_dir(), ZipFile).


strip_comment(Line) ->
    case string:rchr(Line, $#) of
        0   -> Line;
        Pos -> string:sub_string(Line, 1, Pos-1)
    end.


fields(MP, Line) ->
    re:split(Line, MP, [{return, binary}]).

fields_re() ->
    {ok, MP} = re:compile("(?:\\s*;\\s*)|\t"),
    MP.


codepoint(<<"U+",Bin/binary>>) ->
    codepoint(Bin);

codepoint(Bin) ->
    case io_lib:fread("~16u", binary_to_list(Bin)) of
        {ok,[V],[]} ->
            V;
        {ok,[V],Sp} ->
            case lists:any(fun(Ch) -> Ch /= $\s end, Sp) of
                true  -> error({badarg, Bin});
                false -> V
            end;
        _ ->
            error({badarg, Bin})
    end.


codepoint_range(Bin) ->
    case binary:split(Bin, <<"..">>) of
        [CP1, CP2] -> {codepoint(CP1), codepoint(CP2)};
        _          -> error({badarg, Bin})
    end.


codepoint_or_range(Bin) ->
    case binary:split(Bin, <<"..">>) of
        [CP1, CP2] -> {codepoint(CP1), codepoint(CP2)};
        [CP]       -> codepoint(CP)
    end.


codepoints(<<>>) ->
    [];
codepoints(Cps) ->
    [codepoint(Cp) || Cp <- binary:split(Cps, <<" ">>, [global])
                     ,Cp /= <<>>].



compact([]) -> [];
compact([H|T]) ->
    case element(1, H) of
        {Cp1, Cp2} -> compact(T, [{Cp1, Cp2, H}]);
        Cp         -> compact(T, [{Cp, Cp, [H]}])
    end.

compact([], Acc) ->
    lists:foldl(fun ({Start, End, Vs}, Acc0) when is_list(Vs) ->
                        [{Start, End, lists:reverse(Vs)} | Acc0];
                    (V, Acc0) ->
                        [V | Acc0]
                end, [], Acc);

compact([H|T], [{_, _, V} | _] = Acc) when is_tuple(V) ->
    case element(1, H) of
        {Cp1, Cp2} -> compact(T, [{Cp1, Cp2, H} | Acc]);
        Cp         -> compact(T, [{Cp, Cp, [H]} | Acc])
    end;

compact([H|T], [{Cp1, Cp2, Vs}=Range | Acc]) ->
    case element(1, H) of
        {NCp1, NCp2} ->
            compact(T, [{NCp1, NCp2, H}, Range | Acc]);
        Cp when Cp == Cp2 + 1 ->
            compact(T, [{Cp1, Cp, [H|Vs]} | Acc]);
        Cp ->
            compact(T, [{Cp, Cp, [H]}, Range | Acc])
    end.


sort_by_codepoints(Data) -> lists:sort(fun compare_codepoints/2, Data).

compare_codepoints(V1, V2) ->
    compare_codepoints_1(element(1,V1), element(1, V2)).

compare_codepoints_1({_,C1}, {C2,_}) -> C1 =< C2;
compare_codepoints_1({_,C1}, C2)     -> C1 =< C2;
compare_codepoints_1(C1, {C2,_})     -> C1 =< C2;
compare_codepoints_1(C1, C2)         -> C1 =< C2.


to_integer(V) -> list_to_integer(binary_to_list(V)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compact_test_() -> [
  ?_assertEqual([{0, 2, [{0, a}, {1, b}, {2, c}]}
                ,{4, 10, {{4,10}, d}}
                ,{15, 15, [{15, e}]}]
               ,compact([{0, a}
                        ,{1, b}
                        ,{2, c}
                        ,{{4,10}, d}
                        ,{15, e}]))
].

sort_by_codepoints_test_() -> [
  ?_assertEqual([{0, a}
                ,{1, b}
                ,{2, c}
                ,{{4,6}, d}
                ,{{8,10}, e}
                ,{15, f}]
               ,sort_by_codepoints([{{8,10}, e}
                                   ,{15, f}
                                   ,{1, b}
                                   ,{{4,6}, d}
                                   ,{0, a}
                                   ,{2, c}]))
].

-endif.
