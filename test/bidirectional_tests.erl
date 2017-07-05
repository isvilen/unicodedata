-module(bidirectional_tests).
-include_lib("ucd/include/ucd_db.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_TIMEOUT,60).


bidi_test_() -> {timeout, ?TEST_TIMEOUT,
  fun () ->
      ClassToCP = bidi_class_to_codepoint(),
      test_data:fold(
        fun ("@Levels:" ++ L, _File, _LineNo, {_, Reorder}) ->
                {levels(L), Reorder};

            ("@Reorder:" ++ L, _File_, _LineNo, {Levels, _}) ->
                {Levels, reorder(L)};

            (Line, File, LineNo, {Levels, Reorder}) ->
                bidi_test(Line, File, LineNo, ClassToCP, Levels, Reorder),
                {Levels, Reorder}
        end
        ,"BidiTest.txt"
        ,{undefined, undefined})
  end}.


bidi_test(Line, File, LineNo, ClassToCP, Levels, Reorder) ->
    [F0, F1] = string:tokens(Line, ";"),
    Classes = test_data:atoms(F0),
    Bitset = test_data:integer(F1),
    CPs = [maps:get(C, ClassToCP) || C <- Classes],
    OutCPs = reordered_codepoints(CPs, Levels, Reorder),
    bidi_test(File, LineNo, Classes, CPs, (Bitset band 1), Levels, Reorder, OutCPs),
    bidi_test(File, LineNo, Classes, CPs, (Bitset band 2), Levels, Reorder, OutCPs),
    bidi_test(File, LineNo, Classes, CPs, (Bitset band 4), Levels, Reorder, OutCPs).


bidi_test(_File, _LineNo, _Classes, _CPs, 0, _Levels, _Reorder, _OutCPs) ->
    ok;
bidi_test(File, LineNo, _Classes, CPs, PLevel, Levels, Reorder, OutCPs) ->
    P = case PLevel of
            1 -> unicodedata_bidirectional:paragraph(CPs);
            2 -> unicodedata_bidirectional:paragraph(CPs, 0);
            4 -> unicodedata_bidirectional:paragraph(CPs, 1)
        end,
    check(File, LineNo, {levels, {paragraph_level, PLevel}},
          unicodedata_bidirectional:embedding_levels(P), Levels),
    check(File, LineNo, {indices, {paragraph_level, PLevel}},
          unicodedata_bidirectional:reorder_indices(P), Reorder),
    check(File, LineNo, {reorder, {paragraph_level, PLevel}},
          unicodedata_bidirectional:reorder(P), OutCPs).


bidi_character_test_() -> {timeout, ?TEST_TIMEOUT,
  fun () ->
      {ok, MP} = re:compile("(?:\\s*;\\s*)|\t"),
      test_data:foreach(
        fun (Line, File, LineNo) ->
                bidi_character_test(Line, File, LineNo, MP)
        end, "BidiCharacterTest.txt")
  end}.


bidi_character_test(Line, File, LineNo, MP) ->
    [F0, F1, F2, F3, F4] = re:split(Line, MP, [{return, binary}]),
    CPs = test_data:codepoints(F0),
    ParDir = test_data:integer(F1),
    ParEmbLvl = test_data:integer(F2),
    Lvls = levels(F3),
    Idxs = test_data:integers(F4),
    OutCPs = reordered_codepoints(CPs, Lvls, Idxs),
    P = case ParDir of
            2 -> unicodedata_bidirectional:paragraph(CPs);
            _ -> unicodedata_bidirectional:paragraph(CPs, ParDir)
        end,
    check(File, LineNo, embedding_level,
          unicodedata_bidirectional:embedding_level(P), ParEmbLvl),
    check(File, LineNo, levels,
          unicodedata_bidirectional:embedding_levels(P), Lvls),
    check(File, LineNo, indices,
          unicodedata_bidirectional:reorder_indices(P), Idxs),
    check(File, LineNo, reorder,
          unicodedata_bidirectional:reorder(P), OutCPs).


check(File, LineNo, Value, Actual, Expected) ->
    case Actual of
        Expected -> ok;
        _ -> erlang:error({assert, [ {file, File}
                                   , {line, LineNo}
                                   , {value, Value}
                                   , {expected, Expected}
                                   , {actual, Actual}
                                   ]})
    end.


bidi_class_to_codepoint() ->
    Vs = lists:foldl(fun (#unicode_data{code=CP}=V, Acc) when is_integer(CP) ->
                             maps:update_with(V#unicode_data.bidi_class,
                                              fun (CPs) -> [CP | CPs] end,
                                              [CP],
                                              Acc);
                         (_, Acc) ->
                             Acc
                     end, #{}, ucd_db:unicode_data()),
    maps:map(fun (_, CPs) -> test_data:random(CPs) end, Vs).


levels(Data) -> test_data:integers(Data, {"x", hide}).

reorder(Data) -> test_data:integers(Data).


reordered_codepoints(_, [], []) ->
    [];
reordered_codepoints(CPs, [hide|Lvls], Idxs) ->
    reordered_codepoints(CPs, Lvls, Idxs);
reordered_codepoints(CPs, [_|Lvls], [Idx|Idxs]) ->
    [lists:nth(Idx+1, CPs) | reordered_codepoints(CPs, Lvls, Idxs)].
