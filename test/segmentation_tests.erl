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
    test_data:foreach(TestFun, File).


test_fun(File, Fun) ->
    AccFun = fun (LineOrBreak, Acc) -> Acc ++ [LineOrBreak] end,
    BreakFun = fun (Text) -> lists:flatten(Fun(AccFun, [], Text)) end,

    fun ([$@|_], _) ->
            ok;
        (Line, LineNo) ->
            Expected = fields(test_data:strip_comment(Line)),
            Text = [F || F <- Expected, F /= break],
            case BreakFun(Text) of
                Expected -> ok;
                Actual   -> erlang:error({assert, [ {file, File}
                                                  , {line, LineNo}
                                                  , {expected, Expected}
                                                  , {actual, Actual}
                                                  ]})
            end
    end.


fields(Line) ->
    [field(F) || F <- re:split(Line, "\\s+"), F /= <<>>, F /= <<"×"/utf8>>].


field(<<"÷"/utf8>>) -> break;
field(Bin)          -> test_data:codepoint(Bin).
