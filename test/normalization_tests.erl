-module(normalization_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DATA,"NormalizationTest.txt").


quick_check_test_() -> [
    ?_test(foreach_test_data(fun (LineNo, Src, NFC, _, _, _) ->
                                 quick_check_test_form(nfc,  LineNo, Src, NFC)
                             end))
   ,?_test(foreach_test_data(fun (LineNo, Src, _, NFD, _, _) ->
                                 quick_check_test_form(nfd,  LineNo, Src, NFD)
                             end))
   ,?_test(foreach_test_data(fun (LineNo, Src, _, _, NFKC, _) ->
                                 quick_check_test_form(nfkc, LineNo, Src, NFKC)
                             end))
   ,?_test(foreach_test_data(fun (LineNo, Src, _, _, _, NFKD) ->
                                 quick_check_test_form(nfkd, LineNo, Src, NFKD)
                             end))
].

nfc_test_() -> [
    ?_test(foreach_test_data(fun (LineNo, Src, NFC, _, _, _) ->
                                 normalization_check(LineNo, nfc, NFC, Src)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, NFC, _, _, _) ->
                                 normalization_check(LineNo, nfc, NFC, NFC)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, NFC, NFD, _, _) ->
                                 normalization_check(LineNo, nfc, NFC, NFD)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, _, NFKC, _) ->
                                 normalization_check(LineNo, nfc, NFKC, NFKC)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, _, NFKC, NFKD) ->
                                 normalization_check(LineNo, nfc, NFKC, NFKD)
                             end))
].

nfd_test_() -> [
    ?_test(foreach_test_data(fun (LineNo, Src, _, NFD, _, _) ->
                                 normalization_check(LineNo, nfd, NFD, Src)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, NFC, NFD, _, _) ->
                                 normalization_check(LineNo, nfd, NFD, NFC)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, NFD, _, _) ->
                                 normalization_check(LineNo, nfd, NFD, NFD)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, _, NFKC, NFKD) ->
                                 normalization_check(LineNo, nfd, NFKD, NFKC)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, _, _, NFKD) ->
                                 normalization_check(LineNo, nfd, NFKD, NFKD)
                             end))
].

nfkc_test_() -> [
    ?_test(foreach_test_data(fun (LineNo, Src, _, _, NFKC, _) ->
                                 normalization_check(LineNo, nfkc, NFKC, Src)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, NFC, _, NFKC, _) ->
                                 normalization_check(LineNo, nfkc, NFKC, NFC)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, NFD, NFKC, _) ->
                                 normalization_check(LineNo, nfkc, NFKC, NFD)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, _, NFKC, NFKD) ->
                                 normalization_check(LineNo, nfkc, NFKC, NFKD)
                             end))
].

nfkd_test_() -> [
    ?_test(foreach_test_data(fun (LineNo, Src, _, _, _, NFKD) ->
                                 normalization_check(LineNo, nfkd, NFKD, Src)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, NFC, _, _, NFKD) ->
                                 normalization_check(LineNo, nfkd, NFKD, NFC)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, NFD, _, NFKD) ->
                                 normalization_check(LineNo, nfkd, NFKD, NFD)
                             end))

   ,?_test(foreach_test_data(fun (LineNo, _, _, _, NFKC, NFKD) ->
                                 normalization_check(LineNo, nfkd, NFKD, NFKC)
                             end))
].


quick_check_test_form(NormForm, LineNo, Src, Data) ->
    case unicodedata_normalization:quick_check(NormForm, Data) of
        yes   -> ok;
        maybe -> ok;
        no    -> erlang:error({assert, [ {file, ?TEST_DATA}
                                       , {line, LineNo}
                                       , {normalization, NormForm}
                                       , {data, Data}
                                       , {expected, '/= no'}
                                       ]})
    end,
    case unicodedata_normalization:quick_check(NormForm, Src) of
        yes when Src /= Data ->
            erlang:error({assert, [ {file, ?TEST_DATA}
                                  , {line, LineNo}
                                  , {normalization, NormForm}
                                  , {data, Src}
                                  , {expected, '/= yes'}
                                  ]});
        yes   -> ok;
        no    -> ok;
        maybe -> ok
    end.


normalization_check(LineNo, NormForm, Expected, Data) ->
    case unicodedata_normalization:normalize(NormForm, Data) of
        Expected -> ok;
        Actual   -> erlang:error({assert, [ {file, ?TEST_DATA}
                                          , {line, LineNo}
                                          , {normalization, NormForm}
                                          , {source, Data}
                                          , {expected, Expected}
                                          , {actual, Actual}
                                          ]})
    end.


foreach_test_data(Fun) ->
    test_data:foreach(test_fun(Fun), ?TEST_DATA).

test_fun(Fun) ->
    {ok, MP} = re:compile(";"),
    fun ([$@|_], _) ->
            ok;
        (Line, LineNo) ->
            Fields = re:split(Line, MP),
            [Src, NFC, NFD, NFKC, NFKD | _] = Fields,
            Fun(LineNo
               ,test_data:codepoints(Src)
               ,test_data:codepoints(NFC)
               ,test_data:codepoints(NFD)
               ,test_data:codepoints(NFKC)
               ,test_data:codepoints(NFKD))
    end.
