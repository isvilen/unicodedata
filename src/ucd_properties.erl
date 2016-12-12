-module(ucd_properties).
-export([ data/0
        , compact/2
        , ranges/1
        , range_name/1
        , categories/0
        , category_name/1
        , bidi_classes/0
        , bidi_class_name/1
        , bidi_class_defaults/0
        ]).

data() ->
    Data = ucd:fold_lines(fun data/2, "UnicodeData.txt", []),
    lists:reverse(Data).


compact(What, Data) ->
    ucd:compact(filter(What,Data)).


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


data([Cp, Name | _] = Fields, Acc) ->
    case Name of
        <<"<",Name1/binary>> ->
            case binary:match(Name1, <<", Last>">>) of
                {Pos, _} -> data_range(Cp, binary:part(Name1, {0, Pos}), Acc);
                 nomatch -> [data_codepoint(Fields) | Acc]
            end;
        _ ->
            [data_codepoint(Fields) | Acc]
    end.


data_codepoint([Cp, Name, Cat, CombClass, BidiClass, Decomp, Decimal, Digit, Numeric
               ,BidiMirrored, _ , _, Upper, Lower, Title]) ->
    { ucd:codepoint(Cp)
    , Name
    , category(Cat)
    , combining_class(CombClass)
    , bidi_class(BidiClass)
    , decomposition(Decomp)
    , numeric(Decimal, Digit, Numeric)
    , bidi_mirrored(BidiMirrored)
    , case_mapping(Upper)
    , case_mapping(Lower)
    , case_mapping(Title)
    }.

data_range(Cp, Name, [H | T]) ->
    CpFirst = element(1, H),
    H1 = setelement(1, H, {CpFirst, ucd:codepoint(Cp)}),
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
    case ucd:to_integer(CombClass) of
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


decomposition(<<>>) ->
    undefined;
decomposition(Decomp) ->
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

decomposition_1(Mappings) -> [ucd:codepoint(M) || M <- Mappings].


numeric(<<>>, <<>>, <<>>) ->
    undefined;

numeric(<<>>, <<>>, Numeric) ->
    Value = case binary:split(Numeric, <<"/">>) of
                [N1,N2] -> {ucd:to_integer(N1), ucd:to_integer(N2)};
                [N]     -> ucd:to_integer(N)
            end,
    {numeric, Value};

numeric(<<>>, <<Digit:8>>, <<Digit:8>>) ->
    {digit, Digit - $0};

numeric(<<Decimal:8>>, <<Decimal:8>>, <<Decimal:8>>) ->
    {decimal, Decimal - $0}.


bidi_mirrored(<<"Y">>) -> true;
bidi_mirrored(<<"N">>) -> false.


case_mapping(<<>>) -> undefined;
case_mapping(Bin)  -> ucd:codepoint(Bin).


filter(common_properties, Data) ->
    [{Id, Name, Cat, Comb, Bidi, Mirrored}
     || {Id,Name,Cat,Comb,Bidi,_,_,Mirrored,_,_,_} <- Data, is_integer(Id)];

filter(decomposition, Data) ->
    [{Id, Decomp} || {Id,_,_,_,_,Decomp,_,_,_,_,_} <- Data
     , is_integer(Id)
     , Decomp /= undefined];

filter(numeric, Data) ->
    [{Id, Numeric} || {Id,_,_,_,_,_,Numeric,_,_,_,_} <- Data
     , is_integer(Id)
     , Numeric /= undefined];

filter(uppercase_mapping, Data) ->
    [{Id, Upper} || {Id,_,_,_,_,_,_,_,Upper,_,_} <- Data
     , is_integer(Id)
     , Upper /= undefined];

filter(lowercase_mapping, Data) ->
    [{Id, Lower} || {Id,_,_,_,_,_,_,_,_,Lower,_} <- Data
     , is_integer(Id)
     , Lower /= undefined];

filter(titlecase_mapping, Data) ->
    [{Id, Title} || {Id,_,_,_,_,_,_,_,_,_,Title} <- Data
     , is_integer(Id)
     , Title /= undefined].
