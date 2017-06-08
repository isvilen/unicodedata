-module(unicodedata_name).
-include_lib("ucd/include/ucd.hrl").
-export([ blocks/0
        , block/1
        , codepoint_block/1
        , codepoint_name/1
        , codepoint_display_name/1
        , codepoint_lookup/1
        ]).

-spec blocks() -> [{{char(), char()}, binary()}].
blocks() -> ucd:blocks().


-spec block(Name) -> {char(), char()} | no_block
      when Name :: binary() | string().
block(Name) when is_binary(Name) ->
    Blocks = ucd:blocks(),
    case lists:keyfind(Name, 1, Blocks) of
        {Range, _} -> Range;
        false      -> block(ucd:normalize_name(Name), Blocks)
    end;

block(Name) ->
    block(list_to_binary(Name)).

block(_, []) ->
    no_block;

block(Name, [{Range, Block} | Blocks]) ->
    case ucd:normalize_name(Block) of
        Name -> Range;
        _    -> block(Name, Blocks)
    end.


-spec codepoint_block(char()) -> binary() | no_block.
codepoint_block(CP) ->
    case ucd:block(CP) of
        <<"No_Block">> -> no_block;
        V              -> V
    end.


-spec codepoint_name(char()) -> binary().
codepoint_name(CP) ->
    case ucd:name(CP) of
        undefined ->
            case range_name(CP) of
                undefined -> <<"">>;
                Name      -> Name
            end;
        Name ->
            Name
    end.


-spec codepoint_display_name(char()) -> binary().
codepoint_display_name(CP) ->
    case ucd:name_aliases(CP, correction) of
        undefined ->
            case ucd:name(CP) of
                undefined ->
                    case range_name(CP) of
                        undefined -> display_name(CP);
                        Name      -> Name
                    end;
                Name ->
                    Name
            end;
        Name ->
            Name
    end.


-spec codepoint_lookup(binary()) -> char() | not_found.
codepoint_lookup(Name) ->
    case ucd:lookup_name(Name) of
        undefined ->
            case ucd:lookup_aliases(Name, [correction, abbreviation]) of
                undefined ->
                    Tokens = lookup_name_tokens(Name),
                    case range_name_lookup(Tokens) of
                        undefined ->
                            case control_name_lookup(Tokens) of
                                undefined -> not_found;
                                CP        -> CP
                            end;
                        CP ->
                            CP
                    end;
                CP ->
                    CP
            end;
        CP ->
            CP
    end.


range_name(CP) ->
    case ucd:range(CP) of
        {cjk_ideograph, _} -> cjk_ideograph_name(CP);
        cjk_ideograph      -> cjk_ideograph_name(CP);
        tangut_ideograph   -> tangut_ideograph_name(CP);
        hangul_syllable    -> hangul_syllable_name(CP);
        _                  -> undefined
    end.


display_name(CP) ->
    case ucd:range(CP) of
        {high_surrogate, _} -> surrogate_name(CP);
        low_surrogate       -> surrogate_name(CP);
        private_use         -> private_use_name(CP);
        {private_use, _}    -> private_use_name(CP);

        undefined ->
            case ucd:category(CP, 'Cc') of
                true ->
                    control_name(CP);
                false when CP >= 16#FDD0, CP =< 16#FDEF ->
                    noncharacter_name(CP);
                false ->
                    case CP band 16#FFFF of
                        V when V == 16#FFFE
                             ; V == 16#FFFF -> noncharacter_name(CP);
                        _                   -> reserved_name(CP)
                    end
            end
    end.


cjk_ideograph_name(CP) ->
    codepoint_make_name(<<"CJK UNIFIED IDEOGRAPH-">>, CP).


tangut_ideograph_name(CP) ->
    codepoint_make_name(<<"TANGUT IDEOGRAPH-">>, CP).


-define(HANGUL_SYLLABLE_BASE,16#ac00).
-define(HANGUL_SYLLABLES_L,19).
-define(HANGUL_SYLLABLES_V,21).
-define(HANGUL_SYLLABLES_T,28).
-define(HANGUL_SYLLABLES_N,(?HANGUL_SYLLABLES_V * ?HANGUL_SYLLABLES_T)).

hangul_syllable_name(CP) ->
    Idx = CP - ?HANGUL_SYLLABLE_BASE,
    LIdx = Idx div ?HANGUL_SYLLABLES_N,
    VIdx = (Idx rem ?HANGUL_SYLLABLES_N) div ?HANGUL_SYLLABLES_T,
    TIdx = Idx rem ?HANGUL_SYLLABLES_T,
    L = hangul_syllable_L(LIdx),
    V = hangul_syllable_V(VIdx),
    T = hangul_syllable_T(TIdx),
    <<"HANGUL SYLLABLE ",L/binary,V/binary,T/binary>>.

hangul_syllable_L(0)  -> <<"G">>;
hangul_syllable_L(1)  -> <<"GG">>;
hangul_syllable_L(2)  -> <<"N">>;
hangul_syllable_L(3)  -> <<"D">>;
hangul_syllable_L(4)  -> <<"DD">>;
hangul_syllable_L(5)  -> <<"R">>;
hangul_syllable_L(6)  -> <<"M">>;
hangul_syllable_L(7)  -> <<"B">>;
hangul_syllable_L(8)  -> <<"BB">>;
hangul_syllable_L(9)  -> <<"S">>;
hangul_syllable_L(10) -> <<"SS">>;
hangul_syllable_L(11) -> <<>>;
hangul_syllable_L(12) -> <<"J">>;
hangul_syllable_L(13) -> <<"JJ">>;
hangul_syllable_L(14) -> <<"C">>;
hangul_syllable_L(15) -> <<"K">>;
hangul_syllable_L(16) -> <<"T">>;
hangul_syllable_L(17) -> <<"P">>;
hangul_syllable_L(18) -> <<"H">>.


hangul_syllable_V(0)  -> <<"A">>;
hangul_syllable_V(1)  -> <<"AE">>;
hangul_syllable_V(2)  -> <<"YA">>;
hangul_syllable_V(3)  -> <<"YAE">>;
hangul_syllable_V(4)  -> <<"EO">>;
hangul_syllable_V(5)  -> <<"E">>;
hangul_syllable_V(6)  -> <<"YEO">>;
hangul_syllable_V(7)  -> <<"YE">>;
hangul_syllable_V(8)  -> <<"O">>;
hangul_syllable_V(9)  -> <<"WA">>;
hangul_syllable_V(10) -> <<"WAE">>;
hangul_syllable_V(11) -> <<"OE">>;
hangul_syllable_V(12) -> <<"YO">>;
hangul_syllable_V(13) -> <<"U">>;
hangul_syllable_V(14) -> <<"WEO">>;
hangul_syllable_V(15) -> <<"WE">>;
hangul_syllable_V(16) -> <<"WI">>;
hangul_syllable_V(17) -> <<"YU">>;
hangul_syllable_V(18) -> <<"EU">>;
hangul_syllable_V(19) -> <<"YI">>;
hangul_syllable_V(20) -> <<"I">>.


hangul_syllable_T(0)  -> <<>>;
hangul_syllable_T(1)  -> <<"G">>;
hangul_syllable_T(2)  -> <<"GG">>;
hangul_syllable_T(3)  -> <<"GS">>;
hangul_syllable_T(4)  -> <<"N">>;
hangul_syllable_T(5)  -> <<"NJ">>;
hangul_syllable_T(6)  -> <<"NH">>;
hangul_syllable_T(7)  -> <<"D">>;
hangul_syllable_T(8)  -> <<"L">>;
hangul_syllable_T(9)  -> <<"LG">>;
hangul_syllable_T(10) -> <<"LM">>;
hangul_syllable_T(11) -> <<"LB">>;
hangul_syllable_T(12) -> <<"LS">>;
hangul_syllable_T(13) -> <<"LT">>;
hangul_syllable_T(14) -> <<"LP">>;
hangul_syllable_T(15) -> <<"LH">>;
hangul_syllable_T(16) -> <<"M">>;
hangul_syllable_T(17) -> <<"B">>;
hangul_syllable_T(18) -> <<"BS">>;
hangul_syllable_T(19) -> <<"S">>;
hangul_syllable_T(20) -> <<"SS">>;
hangul_syllable_T(21) -> <<"NG">>;
hangul_syllable_T(22) -> <<"J">>;
hangul_syllable_T(23) -> <<"C">>;
hangul_syllable_T(24) -> <<"K">>;
hangul_syllable_T(25) -> <<"T">>;
hangul_syllable_T(26) -> <<"P">>;
hangul_syllable_T(27) -> <<"H">>.


surrogate_name(CP) ->
    codepoint_make_display_name(<<"surrogate-">>, CP).

private_use_name(CP) ->
    codepoint_make_display_name(<<"private-use-">>, CP).

control_name(CP) ->
    case ucd:name_aliases(CP, control) of
        undefined -> codepoint_make_display_name(<<"control-">>, CP);
        [Name|_]  -> iolist_to_binary([$<,Name,$>]);
        Name      -> iolist_to_binary([$<,Name,$>])
    end.

noncharacter_name(CP) ->
    codepoint_make_display_name(<<"noncharacter-">>, CP).

reserved_name(CP) ->
    codepoint_make_display_name(<<"reserved-">>, CP).


lookup_name_tokens(Name) ->
    Name1 = string:lowercase(binary_to_list(Name)),
    string:lexemes(Name1, [$\s,$-]).


range_name_lookup(["cjk", "unified", "ideograph", ID]) ->
    case bin_to_hex(ID) of
        undefined ->
            not_found;
        CP ->
            case ucd:range(CP) of
                {cjk_ideograph, _} -> CP;
                cjk_ideograph      -> CP;
                _                  -> not_found
            end
    end;

range_name_lookup(["tangut", "ideograph", ID]) ->
    case bin_to_hex(ID) of
        undefined ->
            not_found;
        CP ->
            case ucd:range(CP) of
                tangut_ideograph   -> CP;
                _                  -> not_found
            end
    end;


range_name_lookup(["hangul", "syllable" | Rest]) ->
    lookup_hangul_syllable(Rest);

range_name_lookup(_) ->
    undefined.


lookup_hangul_syllable(Tokens) ->
    SYL = iolist_to_binary([string:uppercase(V) || V <- Tokens]),
    case lookup_hangul_syllable_L(SYL) of
        {L, SYL1} ->
            case lookup_hangul_syllable_V(SYL1) of
                {V, SYL2} ->
                    case lookup_hangul_syllable_T(SYL2) of
                        {T, <<>>} ->
                            ?HANGUL_SYLLABLE_BASE
                            + (L * ?HANGUL_SYLLABLES_V + V) * ?HANGUL_SYLLABLES_T
                            + T;
                        _ ->
                            not_found
                    end;
                _ ->
                    not_found
            end;
        _ ->
            not_found
    end.


lookup_hangul_syllable_L(SYL) ->
    lookup_hangul_syllable(SYL, fun hangul_syllable_L/1, ?HANGUL_SYLLABLES_L).

lookup_hangul_syllable_V(SYL) ->
    lookup_hangul_syllable(SYL, fun hangul_syllable_V/1, ?HANGUL_SYLLABLES_V).

lookup_hangul_syllable_T(SYL) ->
    lookup_hangul_syllable(SYL, fun hangul_syllable_T/1, ?HANGUL_SYLLABLES_T).


lookup_hangul_syllable(SYL, Fun, Count) ->
    lookup_hangul_syllable(SYL, 0, Count, Fun, -1, -1).

lookup_hangul_syllable(SYL, Idx, Count, _, SIdx, SLen) when Idx >= Count ->
    if
        SIdx == -1 -> undefined;
        true       -> {SIdx, binary:part(SYL, SLen, byte_size(SYL) - SLen)}
    end;

lookup_hangul_syllable(SYL, Idx, Count, Fun, SIdx, SLen) ->
    S = Fun(Idx),
    Sz = byte_size(S),
    case SYL of
        <<S:Sz/binary,_/binary>> when Sz > SLen ->
            lookup_hangul_syllable(SYL, Idx+1, Count, Fun, Idx, Sz);
        _ ->
            lookup_hangul_syllable(SYL, Idx+1, Count, Fun, SIdx, SLen)
    end.


control_name_lookup(Tokens) ->
    case iolist_to_binary(Tokens) of
        <<$<,Rest/binary>> when byte_size(Rest) > 0 ->
            case binary:last(Rest) of
                $> ->
                    Name = binary:part(Rest, 0, byte_size(Rest)-1),
                    ucd:lookup_aliases(Name, control);
                _ ->
                    undefined
            end;
        Name ->
            ucd:lookup_aliases(Name, control)
    end.


codepoint_make_name(Prefix, CP) when CP =< 16#FFFF ->
    iolist_to_binary([Prefix, io_lib:format("~4.16.0B", [CP])]);

codepoint_make_name(Prefix, CP) ->
    iolist_to_binary([Prefix, io_lib:format("~.16B", [CP])]).


codepoint_make_display_name(Prefix, CP) when CP =< 16#FFFF ->
    iolist_to_binary([$<,Prefix, io_lib:format("~4.16.0B", [CP]),$>]);

codepoint_make_display_name(Prefix, CP) ->
    iolist_to_binary([$<, Prefix, io_lib:format("~.16B", [CP]), $>]).


bin_to_hex(String) ->
    case io_lib:fread("~16u", String) of
        {ok,[V],[]} -> V;
        _           -> undefined
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

blocks_test_() -> [
    ?_assertMatch([{{0,127}, <<"Basic Latin">>} | _], blocks())
   ,?_assertEqual({0,127}, block(<<"Basic Latin">>))
   ,?_assertEqual({16#100, 16#017F}, block("latin extended a"))
   ,?_assertEqual(no_block, block(<<"">>))
].

codepoint_block_test_() -> [
    ?_assertEqual(<<"Basic Latin">>,        codepoint_block(7))
   ,?_assertEqual(<<"Cyrillic">>,           codepoint_block(1029))
   ,?_assertEqual(<<"Letterlike Symbols">>, codepoint_block(8472))
   ,?_assertEqual(<<"Yi Syllables">>,       codepoint_block(41287))
   ,?_assertEqual(<<"Hangul Syllables">>,   codepoint_block(44032))
   ,?_assertEqual(<<"Variation Selectors">>,codepoint_block(65024))
   ,?_assertEqual(<<"Multani">>,            codepoint_block(70277))
   ,?_assertEqual(<<"CJK Unified Ideographs Extension B">>
                 ,codepoint_block(131072))

   ,?_assertEqual(<<"Low Surrogates">>,   codepoint_block(16#dc00))
   ,?_assertEqual(<<"Private Use Area">>, codepoint_block(16#e000))

   ,?_assertEqual(no_block, codepoint_block(16#E00FF))
].

codepoint_name_test_() -> [
    ?_assertEqual(<<"LATIN CAPITAL LETTER A">>, codepoint_name($A))

   ,?_assertEqual(<<"CYRILLIC CAPITAL LETTER DZE">>, codepoint_name(1029))

   ,?_assertEqual(<<"YI SYLLABLE DDOP">>, codepoint_name(41287))

   ,?_assertEqual(<<"SCRIPT CAPITAL P">>, codepoint_name(8472))

   ,?_assertEqual(<<"HANGUL SYLLABLE GA">>, codepoint_name(44032))

   ,?_assertEqual(<<"HANGUL SYLLABLE MIL">>, codepoint_name(48128))

   ,?_assertEqual(<<"TANGUT IDEOGRAPH-17000">>, codepoint_name(94208))

   ,?_assertEqual(<<"CJK UNIFIED IDEOGRAPH-20000">>, codepoint_name(131072))

   ,?_assertEqual(<<"PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET">>
                 ,codepoint_name(16#fe18))

   ,?_assertEqual(<<"">>, codepoint_name(7))

   ,?_assertEqual(<<"">>, codepoint_name(16#d800))

   ,?_assertEqual(<<"">>, codepoint_name(16#db80))

   ,?_assertEqual(<<"">>, codepoint_name(16#dc00))

   ,?_assertEqual(<<"">>, codepoint_name(16#e000))

   ,?_assertEqual(<<"">>, codepoint_name(16#0d80))

   ,?_assertEqual(<<"">>, codepoint_name(16#ffff))
].

codepoint_display_name_test_() -> [
    ?_assertEqual(<<"HANGUL SYLLABLE GA">>, codepoint_display_name(44032))

   ,?_assertEqual(<<"TANGUT IDEOGRAPH-17000">>, codepoint_display_name(94208))

   ,?_assertEqual(<<"CJK UNIFIED IDEOGRAPH-20000">>, codepoint_display_name(131072))

   ,?_assertEqual(<<"PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET">>
                 ,codepoint_display_name(16#fe18))

   ,?_assertEqual(<<"<NULL>">>, codepoint_display_name(0))
   ,?_assertEqual(<<"<LINE FEED>">>, codepoint_display_name($\n))

   ,?_assertEqual(<<"<surrogate-D800>">>, codepoint_display_name(16#d800))

   ,?_assertEqual(<<"<surrogate-DB80>">>, codepoint_display_name(16#db80))

   ,?_assertEqual(<<"<surrogate-DC00>">>, codepoint_display_name(16#dc00))

   ,?_assertEqual(<<"<private-use-E000>">>, codepoint_display_name(16#e000))

   ,?_assertEqual(<<"<reserved-0D80>">>, codepoint_display_name(16#0d80))

   ,?_assertEqual(<<"<noncharacter-FFFF>">>, codepoint_display_name(16#ffff))
].

codepoint_lookup_test_() -> [
    ?_assertEqual($0,    codepoint_lookup(<<"DiGit zERO">>))
   ,?_assertEqual(1029,  codepoint_lookup(<<"CYRILLIC CAPITAL LETTER DZE">>))
   ,?_assertEqual(41287, codepoint_lookup(<<"YI SYLLABLE DDOP">>))

   ,?_assertEqual(16#fe18,
                  codepoint_lookup(<<"PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET">>))
   ,?_assertEqual(16#fe18,
                  codepoint_lookup(<<"PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET">>))
   ,?_assertEqual(8472,  codepoint_lookup(<<"WEIERSTRASS ELLIPTIC FUNCTION">>))

   ,?_assertEqual(71217, codepoint_lookup(<<"MODI VOWEL SIGN I">>))
   ,?_assertEqual(65505, codepoint_lookup(<<"fullwidth pound sign">>))

   ,?_assertEqual(42922, codepoint_lookup(<<"latin-capital-letter-h-with-hook">>))
   ,?_assertEqual(13017, codepoint_lookup(<<"CIRCLED KATAKANA KO">>))

   ,?_assertEqual(16#3400,   codepoint_lookup(<<"CJK UNIFIED IDEOGRAPH-3400">>))
   ,?_assertEqual(16#f9dd,   codepoint_lookup(<<"CJK COMPATIBILITY IDEOGRAPH-F9DD">>))
   ,?_assertEqual(16#20000,  codepoint_lookup(<<"CJK UNIFIED IDEOGRAPH-20000">>))
   ,?_assertEqual(not_found, codepoint_lookup(<<"CJK UNIFIED IDEOGRAPH-F999">>))

   ,?_assertEqual(16#17000, codepoint_lookup(<<"TANGUT IDEOGRAPH-17000">>))

   ,?_assertMatch(16#ac00, codepoint_lookup(<<"HANGUL SYLLABLE GA">>))
   ,?_assertMatch(16#b5bc, codepoint_lookup(<<"HANGUL SYLLABLE DDE">>))
   ,?_assertMatch(16#b7a4, codepoint_lookup(<<"HANGUL SYLLABLE RAELS">>))
   ,?_assertMatch(16#c67f, codepoint_lookup(<<"HANGUL SYLLABLE OED">>))
   ,?_assertMatch(16#c6e8, codepoint_lookup(<<"HANGUL SYLLABLE WE">>))
   ,?_assertMatch(16#d7a3, codepoint_lookup(<<"HANGUL SYLLABLE HIH">>))

   ,?_assertEqual(7,     codepoint_lookup(<<"BEL">>))
   ,?_assertEqual(65024, codepoint_lookup(<<"VS1">>))

   ,?_assertEqual(7,   codepoint_lookup(<<"<ALERT>">>))
   ,?_assertEqual(0,   codepoint_lookup(<<"<NULL>">>))
   ,?_assertEqual($\n, codepoint_lookup(<<"LINE FEED">>))
].

-endif.
