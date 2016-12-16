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
    TestFun = test_fun(File, Fun),
    TestFile = test_data_file(File),
    unicodedata_ucd:fold_lines(TestFun, TestFile, 1, [strip_comment]).


test_fun(File, Fun) ->
    AccFun = fun (LineOrBreak, Acc) -> Acc ++ [LineOrBreak] end,
    BreakFun = fun (Text) -> lists:flatten(Fun(AccFun, [], Text)) end,

    fun ([C|_], LineNo) when C == $#; C == $@ ->
            LineNo + 1;
        (Line, LineNo) ->
            Expected = fields(Line),
            Text = [F || F <- Expected, F /= break],
            case BreakFun(Text) of
                Expected -> ok;
                Actual   -> erlang:error({assert, [ {file, File}
                                                  , {line, LineNo}
                                                  , {expected, Expected}
                                                  , {actual, Actual}
                                                  ]})
            end,
            LineNo + 1
    end.


test_data_file(File) ->
    {_, _, ModuleFile} = code:get_object_code(?MODULE),
    Base = filename:dirname(ModuleFile),
    ZipFile = filename:join([Base, "data", File ++ ".zip"]),
    {ZipFile, File}.


fields(Line) ->
    [field(F) || F <- re:split(Line, "\\s+"), F /= <<>>, F /= <<"×"/utf8>>].


field(<<"÷"/utf8>>) -> break;
field(Bin)          -> unicodedata_ucd:parse_codepoint(Bin).
