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


foreach_test_data(Fun) ->
    IoDev = test_data_iodev(),
    foreach_line(IoDev,
                 fun (Line, LineNo) ->
                         Fields = binary:split(Line, <<";">>, [global]),
                         [Src, NFC, NFD, NFKC, NFKD | _] = Fields,
                         Fun(LineNo
                            ,ucd:codepoints(Src)
                            ,ucd:codepoints(NFC)
                            ,ucd:codepoints(NFD)
                            ,ucd:codepoints(NFKC)
                            ,ucd:codepoints(NFKD))
                 end).


foreach_line(IoDev, Fun) -> foreach_line(IoDev, Fun, 1, io:get_line(IoDev, "")).

foreach_line(_IoDev, _Fun, _LineNo, eof) ->
    ok;

foreach_line(_IoDev, _Fun, _LineNo, {error, Error}) ->
    exit(Error);

foreach_line(IoDev, Fun, LineNo, <<C,_/binary>>) when C == $#; C == $@ ->
    foreach_line(IoDev, Fun, LineNo+1, io:get_line(IoDev, ""));

foreach_line(IoDev, Fun, LineNo, Line) ->
    Fun(Line, LineNo),
    foreach_line(IoDev, Fun, LineNo+1, io:get_line(IoDev, "")).


test_data_iodev() ->
    {_, _, ModuleFile} = code:get_object_code(?MODULE),
    Base = filename:dirname(ModuleFile),
    DataFile = filename:join([Base, "data", ?TEST_DATA]),
    {ok, IoDev} = file:open(DataFile, [read, binary]),
    IoDev.
