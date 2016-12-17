-module(unicodedata_name).
-compile({parse_transform, unicodedata_ucd_transform}).
-export([ blocks/0
        , block/1
        , codepoint_block/1
        , codepoint_name/1
        , codepoint_display_name/1
        ]).

-spec blocks() -> [{binary(), {char(), char()}}].
blocks() -> ucd_blocks().


-spec block(Name) -> {char(), char()} | no_block
      when Name :: binary() | string().
block(Name) when is_binary(Name) ->
    case lists:keyfind(Name, 1, blocks()) of
        {_, Range} -> Range;
        false      -> block(binary_to_list(Name))
    end;

block(Name) ->
    block(normalize_block_name(Name), blocks()).

block(_, []) ->
    no_block;

block(Name, [{Block, Range} | Blocks]) ->
    case normalize_block_name(binary_to_list(Block)) of
        Name -> Range;
        _    -> block(Name, Blocks)
    end.


-spec codepoint_block(char()) -> binary() | no_block.
codepoint_block(CP) -> ucd_block(CP).


-spec codepoint_name(char()) -> binary().
codepoint_name(CP) ->
    case ucd_codepoint_name(CP) of
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
    case ucd_name_aliases(CP, correction) of
        [] ->
            case ucd_codepoint_name(CP) of
                undefined ->
                    case range_name(CP) of
                        undefined -> display_name(CP);
                        Name      -> Name
                    end;
                Name ->
                    Name
            end;
        [Name] ->
            Name
    end.


range_name(CP) ->
    case ucd_codepoint_range(CP) of
        {cjk_ideograph, _} -> cjk_ideograph_name(CP);
        cjk_ideograph      -> cjk_ideograph_name(CP);
        tangut_ideograph   -> tangut_ideograph_name(CP);
        hangul_syllable    -> hangul_syllable_name(CP);
        _                  -> undefined
    end.


display_name(CP) ->
    case ucd_codepoint_range(CP) of
        {high_surrogate, _} -> surrogate_name(CP);
        low_surrogate       -> surrogate_name(CP);
        private_use         -> private_use_name(CP);
        {private_use, _}    -> private_use_name(CP);

        undefined ->
            case ucd_is_category(CP, 'Cc') of
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
    case ucd_name_aliases(CP, control) of
        [Name|_] -> iolist_to_binary([$<,Name,$>]);
        _        -> codepoint_make_display_name(<<"control-">>, CP)
    end.

noncharacter_name(CP) ->
    codepoint_make_display_name(<<"noncharacter-">>, CP).

reserved_name(CP) ->
    codepoint_make_display_name(<<"reserved-">>, CP).


normalize_block_name(Name) ->
    string:to_lower([C || C <- Name, C /= $\s, C /= $-, C /= $_]).


codepoint_make_name(Prefix, CP) when CP =< 16#FFFF ->
    iolist_to_binary([Prefix, io_lib:format("~4.16.0B", [CP])]);

codepoint_make_name(Prefix, CP) ->
    iolist_to_binary([Prefix, io_lib:format("~.16B", [CP])]).


codepoint_make_display_name(Prefix, CP) when CP =< 16#FFFF ->
    iolist_to_binary([$<,Prefix, io_lib:format("~4.16.0B", [CP]),$>]);

codepoint_make_display_name(Prefix, CP) ->
    iolist_to_binary([$<, Prefix, io_lib:format("~.16B", [CP]), $>]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

blocks_test_() -> [
    ?_assertMatch([{<<"Basic Latin">>, {0,127}} | _], blocks())
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
    ?_assertEqual(<<"HANGUL SYLLABLE GA">>, codepoint_name(44032))

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

-endif.
