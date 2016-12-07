-module(ucd).
-export([ files/0
        , file/1
        , fold_lines/3
        , fold_lines/4
        , codepoint/1
        , codepoint_range/1
        , codepoint_or_range/1
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
    filename:dirname(code:where_is_file(?UCD_ZIP)).


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
        {ok,[V],[]} -> V;
        _           -> error({badarg, Bin})
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
