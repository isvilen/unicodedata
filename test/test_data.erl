-module(test_data).
-export([ foreach/2
        , fold/3
        , strip_comment/1
        , codepoint/1
        , codepoints/1
        , integer/1
        , integers/1
        , integers/2
        , atoms/1
        , random/1
        ]).

-include_lib("stdlib/include/zip.hrl").


foreach(Fun, FileName) ->
    Data = file(FileName),
    foreach_1(Fun, FileName, 1, binary:split(Data, <<"\n">>)).


foreach_1(_, _, _, [<<>>]) ->
    ok;

foreach_1(Fun, FileName, LineNo, [Bin]) ->
    foreach_2(Fun, FileName, LineNo, Bin);

foreach_1(Fun, FileName, LineNo, [Bin, Rest]) ->
    foreach_2(Fun, FileName, LineNo, Bin),
    foreach_1(Fun, FileName, LineNo+1, binary:split(Rest, <<"\n">>)).

foreach_2(Fun, FileName, LineNo, Bin) ->
    case line(Bin) of
        [] -> ok;
        Line when is_function(Fun, 2) -> Fun(Line, LineNo);
        Line                          -> Fun(Line, FileName, LineNo)
    end.


fold(Fun, FileName, Acc) ->
    Data = file(FileName),
    fold_1(Fun, FileName, 1, Acc, Data).

fold_1(_, _, _, Acc, <<>>) ->
    Acc;

fold_1(Fun, FileName, LineNo, Acc, Data) ->
    case binary:split(Data, <<"\n">>) of
        [Line] ->
            fold_2(Fun, FileName, LineNo, Acc, Line);
        [Line, Rest] ->
            Acc1 = fold_2(Fun, FileName, LineNo, Acc, Line),
            fold_1(Fun, FileName, LineNo + 1, Acc1, Rest)
    end.

fold_2(Fun, FileName, LineNo, Acc, Bin) ->
    case line(Bin) of
        [] -> Acc;
        Line when is_function(Fun, 3) -> Fun(Line, LineNo, Acc);
        Line                          -> Fun(Line, FileName, LineNo, Acc)
    end.


strip_comment(Line) ->
    {V, _} = string:take(Line, [$#], true),
    V.


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


codepoints(<<>>) ->
    [];
codepoints(Cps) ->
    [codepoint(Cp) || Cp <- binary:split(Cps, <<" ">>, [global]), Cp /= <<>>].


integers(Line, {Except, Value}) ->
    [case string:equal(V, Except) of
         true  -> Value;
         false -> integer(V)
     end || V <- string:lexemes(Line, [$\s, $\t])].


integers(Line) ->
    [integer(T) || T <- string:lexemes(Line, [$\s, $\t])].


atoms(Line) ->
    [list_to_atom(T) || T <- string:lexemes(Line, [$\s, $\t])].


integer(V) when is_binary(V) ->
    list_to_integer(binary_to_list(V));

integer(V) ->
    list_to_integer(string:strip(V)).


random(Vs) ->
    lists:nth(rand:uniform(length(Vs)), Vs).


file({ZipFile, File}) -> file(ZipFile, File);
file(File)            -> file(File, File).

file(ZipFile, File) ->
    {_, _, ModuleFile} = code:get_object_code(?MODULE),
    Base = filename:dirname(ModuleFile),
    ZipFileName = filename:join([Base, "data", ZipFile ++ ".zip"]),
    case zip:unzip(ZipFileName, [{file_list, [File]}, memory]) of
        {ok, [{_, Data}]} -> Data;
        _                 -> error(badarg)
    end.


line(Bin) ->
    case string:chomp(binary_to_list(Bin)) of
        []     -> [];
        [$#|_] -> [];
        Line   -> Line
    end.
