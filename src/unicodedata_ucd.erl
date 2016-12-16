-module(unicodedata_ucd).
-export([ files/0
        , file/1
        , fold_lines/2
        , fold_lines/3
        , fold_lines/4
        , parse_codepoint/1
        , parse_codepoint_range/1
        , parse_codepoint_or_range/1
        , parse_codepoints/1
        , sort_by_codepoints/1
        , parse_integer/1
        ]).

-export([ unicode_data/0
        , codepoints/1
        , ranges/1
        , range_name/1
        , categories/0
        , category_name/1
        , bidi_classes/0
        , bidi_class_name/1
        , bidi_class_defaults/0
        , bidi_mirroring/0
        , prop_list/0
        , prop_list_types/0
        , blocks/0
        , name_aliases/0
        , name_aliases_types/0
        , named_sequences/0
        , east_asian_width/0
        , east_asian_width_values/0
        , east_asian_width_defaults/0
        , hangul_syllable_type/0
        , unihan_numeric_values/0
        , unihan_numeric_types/0
        , case_folding/0
        , special_casing/0
        , composition_exclusions/0
        , derived_normalization_props/0
        , derived_normalization_props_types/0
        , grapheme_break_property/0
        , grapheme_break_classes/0
        , word_break_property/0
        , word_break_classes/0
        , sentence_break_property/0
        , sentence_break_classes/0
        , line_break/0
        , line_break_classes/0
        , non_tailorable_line_break_classes/0
        , line_break_defaults/0
        , line_break_class_name/1
        ]).

-include_lib("stdlib/include/zip.hrl").

-define(UCD_ZIP,"UCD.zip").


files() ->
    lists:flatmap(fun list_files/1, data_files()).


file({ZipFileName, FileName}) -> file(ZipFileName, FileName);
file(FileName)                -> file(zip_file(FileName), FileName).

file(ZipFileName, FileName) ->
    case zip:unzip(ZipFileName, [{file_list, [FileName]}, memory]) of
        {ok, [{_, Data}]} -> Data;
        _                 -> error(badarg)
    end.


fold_lines(Fun, FileName) ->
    Acc1 = fold_lines(fun (L,Acc) -> [Fun(L) | Acc] end, FileName, []),
    lists:reverse(Acc1).


fold_lines(Fun, FileName, Acc) ->
    fold_lines(Fun, FileName, Acc, [skip_empty, strip_comment, fields]).


fold_lines(Fun, FileName, Acc, Opts) ->
    Data = file(FileName),
    StripComment = proplists:get_bool(strip_comment, Opts),
    SplitFields = proplists:get_bool(fields, Opts),
    SkipEmpty = proplists:get_bool(skip_empty, Opts),
    FoldFun = fold_lines_fun(Fun, StripComment, SplitFields),
    fold_lines_1(FoldFun, Acc, binary:split(Data, <<"\n">>), SkipEmpty).


fold_lines_fun(Fun, true, true) ->
    MP = fields_re(),
    fun (Line, Acc) -> Fun(fields(MP, strip_comment(Line)), Acc) end;

fold_lines_fun(Fun, true, false) ->
    fun (Line, Acc) -> Fun(strip_comment(Line), Acc) end;

fold_lines_fun(Fun, false, true) ->
    MP = fields_re(),
    fun (Line, Acc) -> Fun(fields(MP, Line), Acc) end;

fold_lines_fun(Fun, false, false) ->
    Fun.


fold_lines_1(_, Acc, [<<>>], _) ->
    Acc;

fold_lines_1(Fun, Acc0, [Bin, Rest], true) ->
    Acc1 = case string:strip(binary_to_list(Bin), right, $\n) of
               ""       -> Acc0;
               "#" ++ _ -> Acc0;
               Data     -> Fun(Data, Acc0)
           end,
    fold_lines_1(Fun, Acc1, binary:split(Rest, <<"\n">>), true);

fold_lines_1(Fun, Acc0, [Bin, Rest], false) ->
    Data = string:strip(binary_to_list(Bin), right, $\n),
    fold_lines_1(Fun, Fun(Data, Acc0), binary:split(Rest, <<"\n">>), false).


data_dir() ->
    code:priv_dir(unicodedata).


data_files() ->
    DataDir = data_dir(),
    filelib:wildcard(filename:join(DataDir, "*.zip")).


list_files(ZipFile) ->
    {ok, Files} = zip:list_dir(ZipFile),
    [Name || #zip_file{name=Name} <- Files].


zip_file(FileName) ->
    ZipFile = case string:tokens(FileName, [$_]) of
                  [_]          -> ?UCD_ZIP;
                  [Prefix | _] -> Prefix ++ ".zip"
              end,
    filename:join(data_dir(), ZipFile).


strip_comment(Line) ->
    case string:rchr(Line, $#) of
        0   -> Line;
        Pos -> string:sub_string(Line, 1, Pos-1)
    end.


fields(MP, Line) ->
    re:split(Line, MP, [{return, binary}]).

fields_re() ->
    {ok, MP} = re:compile("(?:\\s*;\\s*)|\t"),
    MP.


parse_codepoint(<<"U+",Bin/binary>>) ->
    parse_codepoint(Bin);

parse_codepoint(Bin) ->
    case io_lib:fread("~16u", binary_to_list(Bin)) of
        {ok,[V],[]} ->
            V;
        {ok,[V],Sp} ->
            case lists:any(fun(Ch) -> Ch /= $\s end, Sp) of
                true  -> error({badarg, Bin});
                false -> V
            end;
        _ ->
            error({badarg, Bin})
    end.


parse_codepoint_range(Bin) ->
    case binary:split(Bin, <<"..">>) of
        [CP1, CP2] -> {parse_codepoint(CP1), parse_codepoint(CP2)};
        _          -> error({badarg, Bin})
    end.


parse_codepoint_or_range(Bin) ->
    case binary:split(Bin, <<"..">>) of
        [CP1, CP2] -> {parse_codepoint(CP1), parse_codepoint(CP2)};
        [CP]       -> parse_codepoint(CP)
    end.


parse_codepoints(<<>>) ->
    [];
parse_codepoints(Cps) ->
    [parse_codepoint(Cp) || Cp <- binary:split(Cps, <<" ">>, [global])
                          , Cp /= <<>>].


sort_by_codepoints(Data) -> lists:sort(fun compare_codepoints/2, Data).

compare_codepoints(V1, V2) ->
    compare_codepoints_1(element(1,V1), element(1, V2)).

compare_codepoints_1({_,C1}, {C2,_}) -> C1 =< C2;
compare_codepoints_1({_,C1}, C2)     -> C1 =< C2;
compare_codepoints_1(C1, {C2,_})     -> C1 =< C2;
compare_codepoints_1(C1, C2)         -> C1 =< C2.


parse_integer(V) -> list_to_integer(binary_to_list(V)).


unicode_data() ->
    lists:reverse(fold_lines(fun unicode_data/2, "UnicodeData.txt", [])).


codepoints(Data) ->
    [D || D <- Data, is_integer(element(1,D))].


ranges(Data) ->
    [D || D <- Data, is_tuple(element(1,D))].


range_name(Range) when is_tuple(Range) ->
    range_name(element(2,Range));

range_name(<<"CJK Ideograph">>) ->
    cjk_ideograph;
range_name(<<"CJK Ideograph Extension ",Rest/binary>>) ->
    {cjk_ideograph, {extension, binary_to_list(Rest)}};

range_name(<<"Private Use">>) ->
    private_use;

range_name(<<"Plane ",Id:2/binary," Private Use">>) ->
    {private_use, {plane, list_to_integer(binary_to_list(Id))}};

range_name(<<"Hangul Syllable">>) ->
    hangul_syllable;

range_name(<<"Tangut Ideograph">>) ->
    tangut_ideograph;

range_name(<<"Non Private Use High Surrogate">>) ->
    {high_surrogate, non_private_use};

range_name(<<"Private Use High Surrogate">>) ->
    {high_surrogate, private_use};

range_name(<<"Low Surrogate">>) ->
    low_surrogate.


categories() ->
    ['Lu', 'Ll', 'Lt', 'Lm', 'Lo'
    ,'Mn', 'Mc', 'Me'
    ,'Nd', 'Nl', 'No'
    ,'Pc', 'Pd', 'Ps', 'Pe', 'Pi', 'Pf', 'Po'
    ,'Sm', 'Sc', 'Sk', 'So'
    ,'Zs', 'Zl', 'Zp'
    ,'Cc', 'Cf', 'Cs', 'Co', 'Cn'].


category_name('Lu') -> uppercase_letter;
category_name('Ll') -> lowercase_letter;
category_name('Lt') -> titlecase_letter;

category_name('Lm') -> modifier_letter;
category_name('Lo') -> other_letter;

category_name('Mn') -> monospacing_mark;
category_name('Mc') -> spacing_mark;
category_name('Me') -> enclosing_mark;

category_name('Nd') -> decimal_number;
category_name('Nl') -> letter_number;
category_name('No') -> other_number;

category_name('Pc') -> connector_punctuation;
category_name('Pd') -> dash_punctuation;
category_name('Ps') -> open_punctuation;
category_name('Pe') -> close_punctuation;
category_name('Pi') -> initial_punctuation;
category_name('Pf') -> final_punctuation;
category_name('Po') -> other_punctuation;

category_name('Sm') -> math_symbol;
category_name('Sc') -> currency_symbol;
category_name('Sk') -> modifier_symbol;
category_name('So') -> other_symbol;

category_name('Zs') -> space_separator;
category_name('Zl') -> line_separator;
category_name('Zp') -> paragraph_separator;

category_name('Cc') -> control;
category_name('Cf') -> format;
category_name('Cs') -> surrogate;
category_name('Co') -> private_use;
category_name('Cn') -> unassigned.


bidi_classes() ->
    ['L', 'R', 'AL'
    ,'EN', 'ES', 'ET', 'AN', 'CS', 'NSM', 'BN'
    ,'B', 'S', 'WS', 'ON'
    ,'LRE', 'LRO', 'RLE', 'RLO', 'PDF', 'LRI', 'RLI', 'FSI', 'PDI'].


bidi_class_name('L')   -> left_to_right;
bidi_class_name('R')   -> right_to_left;
bidi_class_name('AL')  -> arabic_letter;

bidi_class_name('EN')  -> european_number;
bidi_class_name('ES')  -> european_separator;
bidi_class_name('ET')  -> european_terminator;
bidi_class_name('AN')  -> arabic_number;
bidi_class_name('CS')  -> common_separator;
bidi_class_name('NSM') -> nonspacing_mark;
bidi_class_name('BN')  -> boundary_neutral;

bidi_class_name('B')  -> paragraph_separator;
bidi_class_name('S')  -> segment_separator;
bidi_class_name('WS') -> white_space;
bidi_class_name('ON') -> other_neutral;

bidi_class_name('LRE') -> left_to_right_embedding;
bidi_class_name('LRO') -> left_to_right_override;
bidi_class_name('RLE') -> right_to_left_embedding;
bidi_class_name('RLO') -> right_to_left_override;
bidi_class_name('PDF') -> pop_directional_format;
bidi_class_name('LRI') -> left_to_right_isolate;
bidi_class_name('RLI') -> right_to_left_isolate;
bidi_class_name('FSI') -> first_strong_isolate;
bidi_class_name('PDI') -> pop_directional_isolate.


%% default values for unassigned codepoints from DerivedBidiClass.txt
bidi_class_defaults() ->
    Ranges = [{16#0600, 16#07BF, 'AL'}
             ,{16#08A0, 16#08FF, 'AL'}
             ,{16#FB50, 16#FDCF, 'AL'}
             ,{16#FDF0, 16#FDFF, 'AL'}
             ,{16#FE70, 16#FEFF, 'AL'}
             ,{16#1EE00, 16#1EEFF, 'AL'}
             ,{16#0590, 16#05FF, 'R'}
             ,{16#07C0, 16#089F, 'R'}
             ,{16#FB1D, 16#FB4F, 'R'}
             ,{16#10800, 16#10FFF, 'R'}
             ,{16#1E800, 16#1EDFF, 'R'}
             ,{16#1EF00, 16#1EFFF, 'R'}
             ,{16#20A0, 16#20CF, 'ET'}
             ],
    lists:sort(fun ({_,V1,_}, {_,V2,_}) -> V1 =< V2 end, Ranges).


bidi_mirroring() ->
    fold_lines(fun ([CP1, CP2])->
                    {parse_codepoint(CP1), parse_codepoint(CP2)}
               end, "BidiMirroring.txt").


prop_list() ->
    fold_lines(fun ([CP, Prop]) ->
                    {parse_codepoint_or_range(CP), prop_list(Prop)}
               end, "PropList.txt").

prop_list(<<"White_Space ">>)             -> white_space;
prop_list(<<"Bidi_Control ">>)            -> bidi_control;
prop_list(<<"Join_Control ">>)            -> join_control;
prop_list(<<"Dash ">>)                    -> dash;
prop_list(<<"Hyphen ">>)                  -> hyphen;
prop_list(<<"Quotation_Mark ">>)          -> quotation_mark;
prop_list(<<"Terminal_Punctuation ">>)    -> terminal_punctuation;
prop_list(<<"Other_Math ">>)              -> other_math;
prop_list(<<"Hex_Digit ">>)               -> hex_digit;
prop_list(<<"ASCII_Hex_Digit ">>)         -> ascii_hex_digit;
prop_list(<<"Other_Alphabetic ">>)        -> other_alphabetic;
prop_list(<<"Ideographic ">>)             -> ideographic;
prop_list(<<"Diacritic ">>)               -> diacritic;
prop_list(<<"Extender ">>)                -> extender;
prop_list(<<"Other_Lowercase ">>)         -> other_lowercase;
prop_list(<<"Other_Uppercase ">>)         -> other_uppercase;
prop_list(<<"Noncharacter_Code_Point ">>) -> noncharacter_code_point;
prop_list(<<"Other_Grapheme_Extend ">>)   -> other_grapheme_extend;
prop_list(<<"IDS_Binary_Operator ">>)     -> ids_binary_operator;
prop_list(<<"IDS_Trinary_Operator ">>)    -> ids_trinary_operator;
prop_list(<<"Radical ">>)                 -> radical;
prop_list(<<"Unified_Ideograph ">>)       -> unified_ideograph;
prop_list(<<"Deprecated ">>)              -> deprecated;
prop_list(<<"Soft_Dotted ">>)             -> soft_dotted;
prop_list(<<"Logical_Order_Exception ">>) -> logical_order_exception;
prop_list(<<"Other_ID_Start ">>)          -> other_id_start;
prop_list(<<"Other_ID_Continue ">>)       -> other_id_continue;
prop_list(<<"Sentence_Terminal ">>)       -> sentence_terminal;
prop_list(<<"Variation_Selector ">>)      -> variation_selector;
prop_list(<<"Pattern_White_Space ">>)     -> pattern_white_space;
prop_list(<<"Pattern_Syntax ">>)          -> pattern_syntax;
prop_list(<<"Prepended_Concatenation_Mark ">>) ->
    prepended_concatenation_mark;
prop_list(<<"Other_Default_Ignorable_Code_Point ">>) ->
    other_default_ignorable_code_point.


prop_list_types() ->
    [ white_space
    , bidi_control
    , join_control
    , dash
    , hyphen
    , quotation_mark
    , terminal_punctuation
    , other_math
    , hex_digit
    , ascii_hex_digit
    , other_alphabetic
    , ideographic
    , diacritic
    , extender
    , other_lowercase
    , other_uppercase
    , noncharacter_code_point
    , other_grapheme_extend
    , ids_binary_operator
    , ids_trinary_operator
    , radical
    , unified_ideograph
    , deprecated
    , soft_dotted
    , logical_order_exception
    , other_id_start
    , other_id_continue
    , sentence_terminal
    , variation_selector
    , pattern_white_space
    , pattern_syntax
    , prepended_concatenation_mark
    , other_default_ignorable_code_point
].


unicode_data([Cp, Name | _] = Fields, Acc) ->
    case Name of
        <<"<",Name1/binary>> ->
            case binary:match(Name1, <<", Last>">>) of
                {Pos, _} ->
                    unicode_data_range(Cp, binary:part(Name1, {0, Pos}), Acc);
                 nomatch ->
                    [unicode_data_codepoint(Fields) | Acc]
            end;
        _ ->
            [unicode_data_codepoint(Fields) | Acc]
    end.


unicode_data_codepoint([Cp, Name, Cat, CombClass, BidiClass, Decomp
                       ,Decimal, Digit, Numeric, BidiMirrored, _ , _
                       ,Upper, Lower, Title]) ->
    { parse_codepoint(Cp)
    , Name
    , category(Cat)
    , combining_class(CombClass)
    , bidi_class(BidiClass)
    , decomposition_data(Decomp)
    , numeric_data(Decimal, Digit, Numeric)
    , bidi_mirrored(BidiMirrored)
    , case_mapping(Upper)
    , case_mapping(Lower)
    , case_mapping(Title)
    }.

unicode_data_range(Cp, Name, [H | T]) ->
    CpFirst = element(1, H),
    H1 = setelement(1, H, {CpFirst, parse_codepoint(Cp)}),
    [setelement(2, H1, Name) | T].


%% LC   Cased_Letter Lu | Ll | Lt
%% L    Letter       Lu | Ll | Lt | Lm | Lo
%% M    Mark         Mn | Mc | Me
%% N    Number       Nd | Nl | No
%% P    Punctuation  Pc | Pd | Ps | Pe | Pi | Pf | Po
%% S    Symbol       Sm | Sc | Sk | So
%% Z    Separator    Zs | Zl | Zp
%% C    Other        Cc | Cf | Cs | Co | Cn
category(Cat) -> erlang:binary_to_existing_atom(Cat, latin1).


combining_class(CombClass) ->
    case parse_integer(CombClass) of
        V when V == 0
             ; V == 1
             ; V == 7
             ; V == 8
             ; V == 9
             ; V >= 10, V =< 133
             ; V == 200
             ; V == 202
             ; V == 214
             ; V == 216
             ; V == 218
             ; V == 220
             ; V == 222
             ; V == 224
             ; V == 226
             ; V == 228
             ; V == 230
             ; V == 232
             ; V == 233
             ; V == 234
             ; V == 240 -> V
    end.


%% Strong Types              L   | R   | AL
%% Weak Types                EN  | ES  | ET  | AN  | CS  | NSM | BN
%% Neutral Types             B   | S   | WS  | ON
%% Explicit Formatting Types LRE | LRO | RLE | RLO | PDF | LRI | RLI | FSI | PDI
bidi_class(BidiClass) -> erlang:binary_to_existing_atom(BidiClass, latin1).


decomposition_data(<<>>) ->
    undefined;
decomposition_data(Decomp) ->
    case binary:split(Decomp, <<" ">>, [global]) of
        [<<"<font>">>     | M] -> {font,     decomposition_1(M)};
        [<<"<noBreak>">>  | M] -> {no_break, decomposition_1(M)};
        [<<"<initial>">>  | M] -> {initial,  decomposition_1(M)};
        [<<"<medial>">>   | M] -> {medial,   decomposition_1(M)};
        [<<"<final>">>    | M] -> {final,    decomposition_1(M)};
        [<<"<isolated>">> | M] -> {isolated, decomposition_1(M)};
        [<<"<circle>">>   | M] -> {circle,   decomposition_1(M)};
        [<<"<super>">>    | M] -> {super,    decomposition_1(M)};
        [<<"<sub>">>      | M] -> {sub,      decomposition_1(M)};
        [<<"<vertical>">> | M] -> {vertical, decomposition_1(M)};
        [<<"<wide>">>     | M] -> {wide,     decomposition_1(M)};
        [<<"<narrow>">>   | M] -> {narrow,   decomposition_1(M)};
        [<<"<small>">>    | M] -> {small,    decomposition_1(M)};
        [<<"<square>">>   | M] -> {square,   decomposition_1(M)};
        [<<"<fraction>">> | M] -> {fraction, decomposition_1(M)};
        [<<"<compat>">>   | M] -> {compat,   decomposition_1(M)};
        M                      -> decomposition_1(M)
    end.

decomposition_1(Mappings) -> [parse_codepoint(M) || M <- Mappings].


numeric_data(<<>>, <<>>, <<>>) ->
    undefined;

numeric_data(<<>>, <<>>, Numeric) ->
    Value = case binary:split(Numeric, <<"/">>) of
                [N1,N2] -> {parse_integer(N1), parse_integer(N2)};
                [N]     -> parse_integer(N)
            end,
    {numeric, Value};

numeric_data(<<>>, <<Digit:8>>, <<Digit:8>>) ->
    {digit, Digit - $0};

numeric_data(<<Decimal:8>>, <<Decimal:8>>, <<Decimal:8>>) ->
    {decimal, Decimal - $0}.


bidi_mirrored(<<"Y">>) -> true;
bidi_mirrored(<<"N">>) -> false.


case_mapping(<<>>) -> undefined;
case_mapping(Bin)  -> parse_codepoint(Bin).


blocks() ->
    fold_lines(fun block_data/1, "Blocks.txt").


name_aliases() ->
    fold_lines(fun name_aliases/1, "NameAliases.txt").


name_aliases_types() ->
    [correction, control, alternate, figment, abbreviation].


named_sequences() ->
    fold_lines(fun named_sequence_data/1, "NamedSequences.txt").


block_data([CpOrRange, Name]) ->
    {parse_codepoint_or_range(CpOrRange), Name}.


name_aliases([Cp,Name,Type]) ->
    {parse_codepoint(Cp), name_alias_type(Type), Name}.

name_alias_type(<<"correction">>)   -> correction;
name_alias_type(<<"control">>)      -> control;
name_alias_type(<<"alternate">>)    -> alternate;
name_alias_type(<<"figment">>)      -> figment;
name_alias_type(<<"abbreviation">>) -> abbreviation.


named_sequence_data([Name, Cps]) ->
    {Name, parse_codepoints(Cps)}.


east_asian_width() ->
    fold_lines(fun east_asian_width_data/1, "EastAsianWidth.txt").


east_asian_width_values() ->
    [ambiguous, full_width, half_width, neutral, narrow, wide].


east_asian_width_defaults() -> [
    {{16#3400, 16#4DBF}, wide}
   ,{{16#4E00, 16#9FFF}, wide}
   ,{{16#F900, 16#FAFF}, wide}
   ,{{16#20000, 16#2FFFD}, wide}
   ,{{16#30000, 16#3FFFD}, wide}
].


east_asian_width_data([CpOrRange, V]) ->
    {parse_codepoint_or_range(CpOrRange), east_asian_width_value(V)}.


east_asian_width_value(<<"A ",_/binary>>) -> ambiguous;
east_asian_width_value(<<"F ",_/binary>>) -> full_width;
east_asian_width_value(<<"H ",_/binary>>) -> half_width;
east_asian_width_value(<<"N ",_/binary>>) -> neutral;
east_asian_width_value(<<"Na ",_/binary>>)-> narrow;
east_asian_width_value(<<"W ",_/binary>>) -> wide.


hangul_syllable_type() ->
    fold_lines(fun hangul_syllable_type_data/1, "HangulSyllableType.txt").

hangul_syllable_type_data([CpOrRange, Type]) ->
    {parse_codepoint_or_range(CpOrRange), hangul_syllable_type(Type)}.

hangul_syllable_type(<<"L ">>)   -> l;
hangul_syllable_type(<<"V ">>)   -> v;
hangul_syllable_type(<<"T ">>)   -> t;
hangul_syllable_type(<<"LV ">>)  -> lv;
hangul_syllable_type(<<"LVT ">>) -> lvt.


unihan_numeric_values() ->
    fold_lines(fun unihan_numeric_data/1, "Unihan_NumericValues.txt").


unihan_numeric_types() ->
    [k_accounting_numeric , k_other_numeric, k_primary_numeric].


unihan_numeric_data([C,T,N]) ->
    {parse_codepoint(C), {unihan_numeric_type(T), parse_integer(N)}}.


unihan_numeric_type(<<"kAccountingNumeric">>) -> k_accounting_numeric;
unihan_numeric_type(<<"kOtherNumeric">>)      -> k_other_numeric;
unihan_numeric_type(<<"kPrimaryNumeric">>)    -> k_primary_numeric.


case_folding() ->
    fold_lines(fun case_folding_data/1, "CaseFolding.txt").


special_casing() ->
    fold_lines(fun special_casing_data/1, "SpecialCasing.txt").


case_folding_data([Cp, <<"C">>, Mapping, <<>>]) ->
    {parse_codepoint(Cp), common, parse_codepoint(Mapping)};

case_folding_data([Cp, <<"S">>, Mapping, <<>>]) ->
    {parse_codepoint(Cp), simple, parse_codepoint(Mapping)};

case_folding_data([Cp, <<"F">>, Mappings, <<>>]) ->
    {parse_codepoint(Cp), full, parse_codepoints(Mappings)};

case_folding_data([Cp, <<"T">>, Mapping, <<>>]) ->
    {parse_codepoint(Cp), turkic, parse_codepoint(Mapping)}.


special_casing_data([CP, Lower, Title, Upper, Conditions, <<>>]) ->
    Cs = [casing_context(C) || C <- binary:split(Conditions, <<" ">>, [global])],
    special_casing_data(CP, Lower, Title, Upper, Cs);

special_casing_data([CP, Lower, Title, Upper, <<>>]) ->
    special_casing_data(CP, Lower, Title, Upper, []).


special_casing_data(CP, Lower, Title, Upper, Conditions) ->
    {parse_codepoint(CP)
    ,parse_codepoints(Lower), parse_codepoints(Title), parse_codepoints(Upper)
    ,Conditions}.


casing_context(<<"Final_Sigma">>)       -> final_sigma;
casing_context(<<"After_Soft_Dotted">>) -> after_soft_dotted;
casing_context(<<"More_Above">>)        -> more_above;
casing_context(<<"Before_Dot">>)        -> before_dot;
casing_context(<<"Not_Before_Dot">>)    -> not_before_dot;
casing_context(<<"After_I">>)           -> after_I;
casing_context(Lang)                    -> Lang.


composition_exclusions() ->
    fold_lines(fun ([CP])-> parse_codepoint(CP) end
              ,"CompositionExclusions.txt").


derived_normalization_props() ->
    Data = fold_lines(fun derived_normalization_props/2
                     ,"DerivedNormalizationProps.txt", []),
    lists:reverse(Data).


derived_normalization_props_types() ->
    [ full_composition_exclusion
    , nfd_quick_check_no
    , nfc_quick_check_no
    , nfc_quick_check_maybe
    , nfkd_quick_check_no
    , nfkc_quick_check_no
    , nfkc_quick_check_maybe
    , nfkc_casefold
    , changes_when_nfkc_casefolded].


derived_normalization_props([_,<<"FC_NFKC">>,_], Acc) ->
    Acc;
derived_normalization_props([V,<<"Full_Composition_Exclusion ">>], Acc) ->
    [{parse_codepoint_or_range(V), full_composition_exclusion} | Acc];

derived_normalization_props([V,<<"NFD_QC">>,<<"N ">>], Acc) ->
    [{parse_codepoint_or_range(V), nfd_quick_check_no} | Acc];

derived_normalization_props([V,<<"NFC_QC">>,<<"N ">>], Acc) ->
    [{parse_codepoint_or_range(V), nfc_quick_check_no} | Acc];

derived_normalization_props([V,<<"NFC_QC">>,<<"M ">>], Acc) ->
    [{parse_codepoint_or_range(V), nfc_quick_check_maybe} | Acc];

derived_normalization_props([V,<<"NFKD_QC">>,<<"N ">>], Acc) ->
    [{parse_codepoint_or_range(V), nfkd_quick_check_no} | Acc];

derived_normalization_props([V,<<"NFKC_QC">>,<<"N ">>], Acc) ->
    [{parse_codepoint_or_range(V), nfkc_quick_check_no} | Acc];

derived_normalization_props([V,<<"NFKC_QC">>,<<"M ">>], Acc) ->
    [{parse_codepoint_or_range(V), nfkc_quick_check_maybe} | Acc];

derived_normalization_props([_, <<"Expands_On_NFD ">>], Acc) ->
    Acc;

derived_normalization_props([_, <<"Expands_On_NFC ">>], Acc) ->
    Acc;

derived_normalization_props([_, <<"Expands_On_NFKD ">>], Acc) ->
    Acc;

derived_normalization_props([_, <<"Expands_On_NFKC ">>], Acc) ->
    Acc;

derived_normalization_props([V,<<"NFKC_CF">>,Cps], Acc) ->
    [{parse_codepoint_or_range(V), nfkc_casefold, parse_codepoints(Cps)} | Acc];

derived_normalization_props([V,<<"Changes_When_NFKC_Casefolded ">>], Acc) ->
    [{parse_codepoint_or_range(V), changes_when_nfkc_casefolded} | Acc].


grapheme_break_property() ->
    fold_lines(fun grapheme_break/1, "GraphemeBreakProperty.txt").

grapheme_break_classes() ->
    [cr, lf, control, extend, zwj, regional_indicator, prepend, spacing_mark,
     l, v, t, lv, lvt, e_base, e_modifier, glue_after_zwj, e_base_gaz].


word_break_property() ->
    fold_lines(fun word_break/1, "WordBreakProperty.txt").

word_break_classes() ->
    [cr, lf, newline, extend, zwj, regional_indicator, format, katakana,
     hebrew_letter, a_letter, single_quote, double_quote, mid_num_let,
     mid_letter, mid_num, numeric, extend_num_let, e_base, e_modifier,
     glue_after_zwj, e_base_gaz].


sentence_break_property() ->
    fold_lines(fun sentence_break/1, "SentenceBreakProperty.txt").

sentence_break_classes() ->
    [cr, lf, extend, sep, format, sp, lower, upper, o_letter, numeric, a_term,
     s_continue, s_term, close].


line_break() ->
    fold_lines(fun line_break/1, "LineBreak.txt").


line_break_classes() -> [
    bk, cm, cr, gl, lf, nl, sg, sp, wj, zw, zwj, % non tailorable
    ai, al, b2, ba, bb, cb, cj, cl, cp, eb,
    em, ex, h2, h3, hl, hy, id, in, is, jl,
    jt, jv, ns, nu, op, po, pr, qu, ri, sa,
    sg, sy, xx
].

non_tailorable_line_break_classes() ->
    [bk, cm, cr, gl, lf, nl, sg, sp, wj, zw, zwj].


line_break_class_name(bk)  -> mandatory_break;
line_break_class_name(cm)  -> combining_mark;
line_break_class_name(cr)  -> carriage_return;
line_break_class_name(gl)  -> non_breaking_glue;
line_break_class_name(lf)  -> line_feed;
line_break_class_name(nl)  -> next_Line;
line_break_class_name(sg)  -> surrogate;
line_break_class_name(sp)  -> space;
line_break_class_name(wj)  -> word_joiner;
line_break_class_name(zw)  -> zero_width_space;
line_break_class_name(zwj) -> zero_width_joiner;
line_break_class_name(ai) -> ambiguous;
line_break_class_name(al) -> alphabetic;
line_break_class_name(b2) -> break_opportunity_before_and_after;
line_break_class_name(ba) -> break_after;
line_break_class_name(bb) -> break_before;
line_break_class_name(cb) -> contingent_break_opportunity;
line_break_class_name(cj) -> conditional_japanese_starter;
line_break_class_name(cl) -> close_punctuation;
line_break_class_name(cp) -> close_parenthesis;
line_break_class_name(eb) -> emoji_base;
line_break_class_name(em) -> emoji_modifier;
line_break_class_name(ex) -> exclamation;
line_break_class_name(h2) -> hangul_lv_syllable;
line_break_class_name(h3) -> hangul_lvt_syllable;
line_break_class_name(hl) -> hebrew_letter;
line_break_class_name(hy) -> hyphen;
line_break_class_name(id) -> ideographic;
line_break_class_name(in) -> inseparable;
line_break_class_name(is) -> infix_numeric_separator;
line_break_class_name(jl) -> hangul_l_jamo;
line_break_class_name(jt) -> hangul_t_jamo;
line_break_class_name(jv) -> hangul_v_jamo;
line_break_class_name(ns) -> nonstarter;
line_break_class_name(nu) -> numeric;
line_break_class_name(op) -> open_punctuation;
line_break_class_name(po) -> postfix_numeric;
line_break_class_name(pr) -> prefix_numeric;
line_break_class_name(qu) -> quotation;
line_break_class_name(ri) -> regional_indicator;
line_break_class_name(sa) -> complex_context_dependent;
line_break_class_name(sy) -> symbols_allowing_break_after;
line_break_class_name(xx) -> other.


line_break_defaults() -> [
    {{16#20A0, 16#20CF}, pr}
   ,{{16#3400, 16#4DBF}, id}
   ,{{16#4E00, 16#9FFF}, id}
   ,{{16#F900, 16#FAFF}, id}
   ,{{16#1F000, 16#1FFFD}, id}
   ,{{16#20000, 16#2FFFD}, id}
   ,{{16#30000, 16#3FFFD}, id}
].


grapheme_break([V,Break]) ->
    {parse_codepoint_or_range(V), grapheme_break_class(Break)}.

grapheme_break_class(<<"CR ">>)                 -> cr;
grapheme_break_class(<<"LF ">>)                 -> lf;
grapheme_break_class(<<"Control ">>)            -> control;
grapheme_break_class(<<"Extend ">>)             -> extend;
grapheme_break_class(<<"ZWJ ">>)                -> zwj;
grapheme_break_class(<<"Regional_Indicator ">>) -> regional_indicator;
grapheme_break_class(<<"Prepend ">>)            -> prepend;
grapheme_break_class(<<"SpacingMark ">>)        -> spacing_mark;
grapheme_break_class(<<"L ">>)                  -> l;
grapheme_break_class(<<"V ">>)                  -> v;
grapheme_break_class(<<"T ">>)                  -> t;
grapheme_break_class(<<"LV ">>)                 -> lv;
grapheme_break_class(<<"LVT ">>)                -> lvt;
grapheme_break_class(<<"E_Base ">>)             -> e_base;
grapheme_break_class(<<"E_Modifier ">>)         -> e_modifier;
grapheme_break_class(<<"Glue_After_Zwj ">>)     -> glue_after_zwj;
grapheme_break_class(<<"E_Base_GAZ ">>)         -> e_base_gaz.


word_break([V,Break]) ->
    {parse_codepoint_or_range(V), word_break_class(Break)}.

word_break_class(<<"CR ">>)                 -> cr;
word_break_class(<<"LF ">>)                 -> lf;
word_break_class(<<"Newline ">>)            -> newline;
word_break_class(<<"Extend ">>)             -> extend;
word_break_class(<<"ZWJ ">>)                -> zwj;
word_break_class(<<"Regional_Indicator ">>) -> regional_indicator;
word_break_class(<<"Format ">>)             -> format;
word_break_class(<<"Katakana ">>)           -> katakana;
word_break_class(<<"Hebrew_Letter ">>)      -> hebrew_letter;
word_break_class(<<"ALetter ">>)            -> a_letter;
word_break_class(<<"Single_Quote ">>)       -> single_quote;
word_break_class(<<"Double_Quote ">>)       -> double_quote;
word_break_class(<<"MidNumLet ">>)          -> mid_num_let;
word_break_class(<<"MidLetter ">>)          -> mid_letter;
word_break_class(<<"MidNum ">>)             -> mid_num;
word_break_class(<<"Numeric ">>)            -> numeric;
word_break_class(<<"ExtendNumLet ">>)       -> extend_num_let;
word_break_class(<<"E_Base ">>)             -> e_base;
word_break_class(<<"E_Modifier ">>)         -> e_modifier;
word_break_class(<<"Glue_After_Zwj ">>)     -> glue_after_zwj;
word_break_class(<<"E_Base_GAZ ">>)         -> e_base_gaz.


sentence_break([V,Break]) ->
    {parse_codepoint_or_range(V), sentence_break_class(Break)}.

sentence_break_class(<<"CR ">>)        -> cr;
sentence_break_class(<<"LF ">>)        -> lf;
sentence_break_class(<<"Extend ">>)    -> extend;
sentence_break_class(<<"Sep ">>)       -> sep;
sentence_break_class(<<"Format ">>)    -> format;
sentence_break_class(<<"Sp ">>)        -> sp;
sentence_break_class(<<"Lower ">>)     -> lower;
sentence_break_class(<<"Upper ">>)     -> upper;
sentence_break_class(<<"OLetter ">>)   -> o_letter;
sentence_break_class(<<"Numeric ">>)   -> numeric;
sentence_break_class(<<"ATerm ">>)     -> a_term;
sentence_break_class(<<"SContinue ">>) -> s_continue;
sentence_break_class(<<"STerm ">>)     -> s_term;
sentence_break_class(<<"Close ">>)     -> close.


line_break([V,Break]) ->
    {parse_codepoint_or_range(V), line_break_class(Break)}.

line_break_class(<<"BK ",_/binary>>)  -> bk;
line_break_class(<<"CM ",_/binary>>)  -> cm;
line_break_class(<<"CR ",_/binary>>)  -> cr;
line_break_class(<<"GL ",_/binary>>)  -> gl;
line_break_class(<<"LF ",_/binary>>)  -> lf;
line_break_class(<<"NL ",_/binary>>)  -> nl;
line_break_class(<<"SP ",_/binary>>)  -> sp;
line_break_class(<<"WJ ",_/binary>>)  -> wj;
line_break_class(<<"ZW ",_/binary>>)  -> zw;
line_break_class(<<"ZWJ ",_/binary>>) -> zwj;
line_break_class(<<"AI ",_/binary>>) -> ai;
line_break_class(<<"AL ",_/binary>>) -> al;
line_break_class(<<"B2 ",_/binary>>) -> b2;
line_break_class(<<"BA ",_/binary>>) -> ba;
line_break_class(<<"BB ",_/binary>>) -> bb;
line_break_class(<<"CB ",_/binary>>) -> cb;
line_break_class(<<"CJ ",_/binary>>) -> cj;
line_break_class(<<"CL ",_/binary>>) -> cl;
line_break_class(<<"CP ",_/binary>>) -> cp;
line_break_class(<<"EB ",_/binary>>) -> eb;
line_break_class(<<"EM ",_/binary>>) -> em;
line_break_class(<<"EX ",_/binary>>) -> ex;
line_break_class(<<"H2 ",_/binary>>) -> h2;
line_break_class(<<"H3 ",_/binary>>) -> h3;
line_break_class(<<"HL ",_/binary>>) -> hl;
line_break_class(<<"HY ",_/binary>>) -> hy;
line_break_class(<<"ID ",_/binary>>) -> id;
line_break_class(<<"IN ",_/binary>>) -> in;
line_break_class(<<"IS ",_/binary>>) -> is;
line_break_class(<<"JL ",_/binary>>) -> jl;
line_break_class(<<"JT ",_/binary>>) -> jt;
line_break_class(<<"JV ",_/binary>>) -> jv;
line_break_class(<<"NS ",_/binary>>) -> ns;
line_break_class(<<"NU ",_/binary>>) -> nu;
line_break_class(<<"OP ",_/binary>>) -> op;
line_break_class(<<"PO ",_/binary>>) -> po;
line_break_class(<<"PR ",_/binary>>) -> pr;
line_break_class(<<"QU ",_/binary>>) -> qu;
line_break_class(<<"RI ",_/binary>>) -> ri;
line_break_class(<<"SA ",_/binary>>) -> sa;
line_break_class(<<"SG ",_/binary>>) -> sg;
line_break_class(<<"SY ",_/binary>>) -> sy;
line_break_class(<<"XX ",_/binary>>) -> xx.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sort_by_codepoints_test_() -> [
  ?_assertEqual([{0, a}
                ,{1, b}
                ,{2, c}
                ,{{4,6}, d}
                ,{{8,10}, e}
                ,{15, f}]
               ,sort_by_codepoints([{{8,10}, e}
                                   ,{15, f}
                                   ,{1, b}
                                   ,{{4,6}, d}
                                   ,{0, a}
                                   ,{2, c}]))
].

-endif.
