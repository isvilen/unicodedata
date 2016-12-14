-module(unicodedata_normalization).
-compile({parse_transform, ucd_transform}).
-export([ canonical_decomposition/1
        , compatibility_decomposition/1
        , canonical_ordering/1
        ]).

-spec canonical_decomposition(string()) -> string().
canonical_decomposition(String) ->
    canonical_decomposition_1(String, []).

canonical_decomposition_1([], Acc) ->
    lists:reverse(Acc);

canonical_decomposition_1([CP | CPs], AccIn) ->
    AccOut =
        case decomposition(CP) of
            DCPs when is_list(DCPs) -> push(canonical_decomposition(DCPs), AccIn);
            _                       -> [CP | AccIn]
        end,
    canonical_decomposition_1(CPs, AccOut).


-spec compatibility_decomposition(string()) -> string().
compatibility_decomposition(String) ->
    compatibility_decomposition_1(String, []).

compatibility_decomposition_1([], Acc) ->
    lists:reverse(Acc);

compatibility_decomposition_1([CP | CPs], AccIn) ->
    AccOut =
        case decomposition(CP) of
            undefined -> [CP | AccIn];
            {_, DCPs} -> push(compatibility_decomposition(DCPs), AccIn);
            DCPs      -> push(compatibility_decomposition(DCPs), AccIn)
        end,
    compatibility_decomposition_1(CPs, AccOut).


-spec canonical_ordering(string()) -> string().
canonical_ordering(String) ->
    canonical_ordering_1(String, [], []).

canonical_ordering_1([], Acc1, Acc2) ->
    lists:reverse(canonical_sort(Acc2) ++ Acc1);

canonical_ordering_1([CP | CPs], Acc1, Acc2) ->
    case ucd_combining_class(CP) of
        0 when Acc2 == [] ->
            canonical_ordering_1(CPs, [CP | Acc1], []);
        0 ->
            NewAcc1 = canonical_sort(Acc2) ++ Acc1,
            canonical_ordering_1(CPs, [CP | NewAcc1], []);
        CC ->
            canonical_ordering_1(CPs, Acc1, [{-CC, CP} | Acc2])
    end.

canonical_sort(Acc) ->
    [CP || {_, CP} <- lists:keysort(1, Acc)].


decomposition(CP) ->
    case ucd_decomposition(CP) of
        undefined ->
            case ucd_hangul_syllable_type(CP) of
                lv  -> hangul_syllable_decomposition_lv(CP);
                lvt -> hangul_syllable_decomposition_lvt(CP);
                _   -> undefined
            end;
        Value ->
            Value
    end.


-define(HANGUL_SYLLABLE_BASE,16#ac00).
-define(HANGUL_SYLLABLE_L_BASE,16#1100).
-define(HANGUL_SYLLABLE_V_BASE,16#1161).
-define(HANGUL_SYLLABLE_T_BASE,16#11a7).

-define(HANGUL_SYLLABLES_L,19).
-define(HANGUL_SYLLABLES_V,21).
-define(HANGUL_SYLLABLES_T,28).
-define(HANGUL_SYLLABLES_N,(?HANGUL_SYLLABLES_V * ?HANGUL_SYLLABLES_T)).
-define(HANGUL_SYLLABLES_COUNT,(?HANGUL_SYLLABLES_L * ?HANGUL_SYLLABLES_N)).

hangul_syllable_decomposition_lv(CP) ->
    Idx = CP - ?HANGUL_SYLLABLE_BASE,
    LIdx = Idx div ?HANGUL_SYLLABLES_N,
    VIdx = (Idx rem ?HANGUL_SYLLABLES_N) div ?HANGUL_SYLLABLES_T,
    [?HANGUL_SYLLABLE_L_BASE + LIdx, ?HANGUL_SYLLABLE_V_BASE + VIdx].


hangul_syllable_decomposition_lvt(CP) ->
    Idx = CP - ?HANGUL_SYLLABLE_BASE,
    LVIdx = (Idx div ?HANGUL_SYLLABLES_T) * ?HANGUL_SYLLABLES_T,
    TIdx = Idx rem ?HANGUL_SYLLABLES_T,
    [?HANGUL_SYLLABLE_BASE + LVIdx, ?HANGUL_SYLLABLE_T_BASE + TIdx].


push([], Chars)       -> Chars;
push([C | Cs], Chars) -> push(Cs, [C | Chars]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

canonical_decomposition_test_() -> [
    ?_assertEqual("Ḋ",                 canonical_decomposition("Ḋ"))
   ,?_assertEqual("Ḋ",                 canonical_decomposition("Ḋ"))

   ,?_assertEqual("A\x{0308}ffin",     canonical_decomposition("Äffin"))
   ,?_assertEqual("A\x{0308}\x{FB03}n",canonical_decomposition("Ä\x{FB03}n"))

   ,?_assertEqual("Henry IV",          canonical_decomposition("Henry IV"))
   ,?_assertEqual("Henry \x{2163}",    canonical_decomposition("Henry \x{2163}"))

   ,?_assertEqual("ガ",               canonical_decomposition("ガ"))
   ,?_assertEqual("가",               canonical_decomposition("가"))
   ,?_assertEqual("뗣",             canonical_decomposition("뗣"))
].


compatibility_decomposition_test_() -> [
    ?_assertEqual("Ḋ", compatibility_decomposition("Ḋ"))
   ,?_assertEqual("Ḋ", compatibility_decomposition("Ḋ"))

   ,?_assertEqual("A\x{0308}ffin",compatibility_decomposition("Äffin"))
   ,?_assertEqual("A\x{0308}ffin",compatibility_decomposition("Ä\x{FB03}n"))

   ,?_assertEqual("Henry IV",compatibility_decomposition("Henry IV"))
   ,?_assertEqual("Henry IV",compatibility_decomposition("Henry \x{2163}"))

   ,?_assertEqual("ガ",   compatibility_decomposition("ガ"))
   ,?_assertEqual("가",   compatibility_decomposition("가"))
   ,?_assertEqual("뗣", compatibility_decomposition("뗣"))
].


canonical_ordering_test_() -> [
    ?_assertEqual(                   [68,775],
                  canonical_ordering([68,775]))

   ,?_assertEqual(                   [68,803,775],
                  canonical_ordering([68,803,775]))

   ,?_assertEqual(                   [68,803,775],
                  canonical_ordering([68,775,803]))

   ,?_assertEqual(                   [68,795,803,775],
                  canonical_ordering([68,775,795,803]))

   ,?_assertEqual(                   [65,66,775,67,68],
                  canonical_ordering([65,66,775,67,68]))

   ,?_assertEqual(                   [65,66,803,775,67,68],
                  canonical_ordering([65,66,803,775,67,68]))

   ,?_assertEqual(                   [65,66,803,775,67,68],
                  canonical_ordering([65,66,775,803,67,68]))

   ,?_assertEqual(                   [65,66,795,803,775,67,68],
                  canonical_ordering([65,66,775,795,803,67,68]))
].

-endif.
