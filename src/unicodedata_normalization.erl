-module(unicodedata_normalization).
-compile({parse_transform, ucd_transform}).
-export([ canonical_decomposition/1
        , compatibility_decomposition/1
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

-endif.
