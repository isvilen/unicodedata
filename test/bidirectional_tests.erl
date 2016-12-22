-module(bidirectional_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_TIMEOUT,60).


bidi_test_() -> {timeout, ?TEST_TIMEOUT,
  fun () ->
      File = "BidiTest.txt",
      ClassToCP = bidi_class_to_codepoint(),
      unicodedata_ucd:fold_lines(
        fun ("@Levels:" ++ L, {LineNo, _, Reorder}) ->
            {LineNo + 1, parse_levels(L), Reorder};

            ("@Reorder:" ++ L, {LineNo, Levels, _}) ->
            {LineNo + 1, Levels, parse_indexes(L)};

            (Line, {LineNo, Levels, Reorder}) when Line == []
                                                 ; hd(Line) == $#
                                                 ; hd(Line) == $@ ->
            {LineNo + 1, Levels, Reorder};

            (Line, {LineNo, Levels, Reorder}) ->
                [F0, F1] = string:tokens(Line, ";"),
                Classes = parse_bidi_classes(F0),
                Bitset = parse_integer(F1),
                CPs = codepoints_from_bidi_classes(Classes, ClassToCP),
                bidi_test(File, LineNo, Classes, CPs, Bitset, Levels, Reorder),
                {LineNo + 1, Levels, Reorder}
        end
        ,test_data_file(File)
        ,{1, undefined, undefined}
        ,[])
  end}.


bidi_test(File, LineNo, Classes, CPs, Bitset, Levels, Reorder) ->
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
      File = "BidiCharacterTest.txt",
      unicodedata_ucd:fold_lines(
        fun ([F0, F1, F2, F3, F4], LineNo) ->
            CPs = unicodedata_ucd:parse_codepoints(F0),
            ParDir = parse_integer(F1),
            ParEmbLvl = parse_integer(F2),
            Lvls = parse_levels(F3),
            Idxs = parse_indexes(F4),
            bidi_character_test(File, LineNo, CPs, ParDir, ParEmbLvl, Lvls, Idxs),
            LineNo + 1;
            (_, LineNo) -> LineNo + 1
        end
        ,test_data_file(File)
        ,1
        ,[fields])
  end}.


bidi_character_test(File, LineNo, CPs, ParDir, ParEmbLvl, Lvls, Idxs) ->
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


test_data_file(TestFile) ->
    {_, _, ModuleFile} = code:get_object_code(?MODULE),
    Base = filename:dirname(ModuleFile),
    ZipFile = filename:join([Base, "data", "BidiTest.zip"]),
    {ZipFile, TestFile}.


bidi_class_to_codepoint() ->
    select_codepoints(collect_codepoints()).

collect_codepoints() ->
    UnicodeData = unicodedata_ucd:unicode_data(),
    lists:foldl(fun ({CP,_,_,_,BidiCs,_,_,_,_,_,_} , Acc) ->
                     maps:update_with(BidiCs,fun (CPs) -> [CP|CPs] end,[CP],Acc)
                end, #{}, unicodedata_ucd:codepoints(UnicodeData)).

select_codepoints(ClassToCPs) ->
    maps:map(fun (_, CPs) ->
                 lists:nth(rand:uniform(length(CPs)), CPs)
             end, ClassToCPs).


codepoints_from_bidi_classes(Classes, ClassToCPs) ->
    [maps:get(C, ClassToCPs) || C <- Classes].


parse_levels(Bin) when is_binary(Bin) ->
    [case B of <<"x">> -> hide; _ -> parse_integer(B) end
     || B <- binary:split(Bin, <<" ">>, [global])];

parse_levels(Line) ->
    [case T of "x" -> hide; _ -> list_to_integer(T) end
     || T <- string:tokens(Line, [$\s, $\t])].


parse_indexes(Bin) when is_binary(Bin) ->
    [parse_integer(B) || B <- binary:split(Bin, <<" ">>, [global])];

parse_indexes(Line) ->
    [list_to_integer(T) || T <- string:tokens(Line, [$\s, $\t])].


parse_bidi_classes(Data) ->
    [list_to_atom(T) || T <- string:tokens(Data, " ")].


parse_integer(V) when is_binary(V) ->
    list_to_integer(binary_to_list(V));

parse_integer(V) ->
    list_to_integer(string:strip(V)).


reordered_codepoints(_, [], []) ->
    [];
reordered_codepoints(CPs, [hide|Lvls], Idxs) ->
    reordered_codepoints(CPs, Lvls, Idxs);
reordered_codepoints(CPs, [_|Lvls], [Idx|Idxs]) ->
    [lists:nth(Idx+1, CPs) | reordered_codepoints(CPs, Lvls, Idxs)].
