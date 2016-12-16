-module(segmentation_tests).
-include_lib("eunit/include/eunit.hrl").


grapheme_breaks_test() ->
    run_tests("GraphemeBreakTest.txt",
              fun unicodedata_segmentation:grapheme_breaks/3).


word_breaks_test() ->
    run_tests("WordBreakTest.txt",
              fun unicodedata_segmentation:word_breaks/3).


sentence_breaks_test() ->
    run_tests("SentenceBreakTest.txt",
              fun unicodedata_segmentation:sentence_breaks/3).


line_breaks_test() ->
    run_tests("LineBreakTest.txt",
              fun unicodedata_segmentation:line_breaks/3).


run_tests(File, Fun) ->
    AccFun = fun (LineOrBreak, Acc) -> Acc ++ [LineOrBreak] end,
    BreakFun = fun (Text) -> lists:flatten(Fun(AccFun, [], Text)) end,
    foreach_line(test_data_iodev(File),
                 fun (LineNo, Expected, Text) ->
                         case BreakFun(Text) of
                             Expected ->
                                 ok;
                             Actual ->
                                 erlang:error({assert, [ {file, File}
                                                       , {line, LineNo}
                                                       , {expected, Expected}
                                                       , {actual, Actual}
                                                       ]})
                         end
                 end).


foreach_line(IoDev, Fun) -> foreach_line(IoDev, Fun, 1, io:get_line(IoDev, "")).

foreach_line(_IoDev, _Fun, _LineNo, eof) ->
    ok;

foreach_line(_IoDev, _Fun, _LineNo, {error, Error}) ->
    exit(Error);

foreach_line(IoDev, Fun, LineNo, [C | _]) when C == $#; C == $@ ->
    foreach_line(IoDev, Fun, LineNo+1, io:get_line(IoDev, ""));

foreach_line(IoDev, Fun, LineNo, Line) ->
    Expected = fields(strip_comment(Line)),
    Text = [F || F <- Expected, F /= break],
    Fun(LineNo, Expected, Text),
    foreach_line(IoDev, Fun, LineNo+1, io:get_line(IoDev, "")).


test_data_iodev(File) ->
    {_, _, ModuleFile} = code:get_object_code(?MODULE),
    Base = filename:dirname(ModuleFile),
    DataFile = filename:join([Base, "data", File]),
    {ok, IoDev} = file:open(DataFile, [read]),
    IoDev.


strip_comment(Line) ->
    case string:rchr(Line, $#) of
        0   -> Line;
        Pos -> string:sub_string(Line, 1, Pos-1)
    end.


fields(Line) ->
    [field(F) || F <- re:split(Line, "\\s+"), F /= <<>>, F /= <<"×"/utf8>>].


field(<<"÷"/utf8>>) -> break;
field(Bin)          -> unicodedata_ucd:parse_codepoint(Bin).
