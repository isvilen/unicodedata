-module(unicodedata_normalization).
-include_lib("ucd/include/ucd.hrl").
-export([ quick_check/2
        , normalize/2
        , canonical_decomposition/1
        , compatibility_decomposition/1
        , canonical_composition/1
        , canonical_ordering/1
        ]).

-type normalization_form() :: nfc
                            | nfkc
                            | nfd
                            | nfkd.

-export_type([normalization_form/0]).


-spec quick_check(normalization_form(), string()) -> yes | no | maybe.
quick_check(Form, String) ->
    quick_check_1(Form, String, 0, yes).


quick_check_1(_, [], _, Result) ->
    Result;

quick_check_1(Form, [CP | CPs], LastClass, Result) ->
    case ucd:combining_class(CP) of
        Class when LastClass > Class, Class /= 0 -> no;
        Class -> case quick_check_2(Form, CP) of
                     no    -> no;
                     maybe -> quick_check_1(Form, CPs, Class, maybe);
                     _     -> quick_check_1(Form, CPs, Class, Result)
                 end
    end.

quick_check_2(nfc, CP)  -> ucd:nfc_quick_check(CP);
quick_check_2(nfkc, CP) -> ucd:nfkc_quick_check(CP);
quick_check_2(nfd, CP)  -> ucd:nfd_quick_check(CP);
quick_check_2(nfkd, CP) -> ucd:nfkd_quick_check(CP).


-spec normalize(normalization_form(), string()) -> string().
normalize(Form, String) ->
    case quick_check(Form, String) of
        yes -> String;
        _   -> normalize_1(Form, String)
   end.

normalize_1(nfc, String) ->
    String1 = canonical_decomposition(String),
    String2 = canonical_ordering(String1),
    canonical_composition(String2);

normalize_1(nfd, String) ->
    String1 = canonical_decomposition(String),
    canonical_ordering(String1);

normalize_1(nfkc, String) ->
    String1 = compatibility_decomposition(String),
    String2 = canonical_ordering(String1),
    canonical_composition(String2);

normalize_1(nfkd, String) ->
    String1 = compatibility_decomposition(String),
    canonical_ordering(String1).


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


-spec canonical_composition(string()) -> string().
canonical_composition(String) ->
    canonical_composition_1(String, undefined, [], []).


canonical_composition_1([], undefined, [], Acc) ->
    lists:reverse(Acc);

canonical_composition_1([], Starter, DecAcc, Acc) ->
    lists:reverse(push([Starter | lists:reverse(DecAcc)], Acc));

canonical_composition_1([CP | CPs], undefined, [], Acc) ->
    case ucd:combining_class(CP) of
        0 -> canonical_composition_1(CPs, CP, [], Acc);
        _ -> canonical_composition_1(CPs, undefined, [], [CP |Acc])
    end;

canonical_composition_1([CP | CPs], Starter, [], Acc) ->
    case composition(Starter, CP) of
        undefined ->
            case ucd:combining_class(CP) of
                0 ->
                    canonical_composition_1(CPs, CP, [], [Starter | Acc]);
                _ ->
                    canonical_composition_1(CPs, Starter, [CP], Acc)
            end;
        CCP ->
            canonical_composition_1(CPs, CCP, [], Acc)
    end;

canonical_composition_1([CP | CPs], Starter, [DCP | _] = DecAcc, Acc) ->
    CP_CCC = ucd:combining_class(CP),
    DCP_CCC = ucd:combining_class(DCP),

    case DCP_CCC >= CP_CCC of
        true when CP_CCC == 0 ->
            Acc1 = push([Starter | lists:reverse(DecAcc)], Acc),
            canonical_composition_1(CPs, CP, [], Acc1);
        true  ->
            canonical_composition_1(CPs, Starter, [CP | DecAcc], Acc);
        false ->
            case composition(Starter, CP) of
                undefined when CP_CCC == 0 ->
                    Acc1 = push([Starter | lists:reverse(DecAcc)], Acc),
                    canonical_composition_1(CPs, CP, [], Acc1);
                undefined ->
                    canonical_composition_1(CPs, Starter, [CP|DecAcc], Acc);
                CCP ->
                    canonical_composition_1(CPs, CCP, DecAcc, Acc)
            end
    end.


-spec canonical_ordering(string()) -> string().
canonical_ordering(String) ->
    canonical_ordering_1(String, [], []).

canonical_ordering_1([], Acc1, Acc2) ->
    lists:reverse(canonical_sort(Acc2) ++ Acc1);

canonical_ordering_1([CP | CPs], Acc1, Acc2) ->
    case ucd:combining_class(CP) of
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
    case ucd:decomposition(CP) of
        undefined ->
            case ucd:hangul_syllable_type(CP) of
                lv  -> hangul_syllable_decomposition_lv(CP);
                lvt -> hangul_syllable_decomposition_lvt(CP);
                _   -> undefined
            end;
        Value ->
            Value
    end.


composition(CP1, CP2) ->
    case ucd:composition(CP1, CP2) of
        undefined -> hangul_syllable_composition(CP1, CP2);
        Value     -> Value
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


hangul_syllable_composition(CP1, CP2) when CP1 >= 16#1100, CP1 =< 16#1112
                                         , CP2 >= 16#1161, CP2 =< 16#1175 ->
    LIdx = CP1 - ?HANGUL_SYLLABLE_L_BASE,
    VIdx = CP2 - ?HANGUL_SYLLABLE_V_BASE,
    LVIdx = LIdx * ?HANGUL_SYLLABLES_N + VIdx * ?HANGUL_SYLLABLES_T,
    ?HANGUL_SYLLABLE_BASE + LVIdx;

hangul_syllable_composition(CP1, CP2) ->
    case ucd:hangul_syllable_type(CP1) of
        lv when CP2 >= 16#11a8, CP2 =< 16#11c2 ->
            TIdx = CP2 - ?HANGUL_SYLLABLE_T_BASE,
            CP1 + TIdx;
        _ ->
            undefined
    end.


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
