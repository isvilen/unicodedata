-module(ucd_codegen).
-export([ index_fun_ast/2
        , range_fun_ast/2
        , range_fun_ast/3
        , data_fun_ast/2
        , data_fun_ast/3
        , common_properties_index_fun_ast/1
        , category_data_fun_ast/1
        , category_range_data_fun_ast/1
        , category_fun_ast/0
        , is_category_fun_ast/3
        , has_property_fun_ast/3
        , combining_class_data_fun_ast/1
        , combining_class_range_data_fun_ast/1
        , combining_class_fun_ast/0
        , bidi_class_data_fun_ast/1
        , bidi_class_range_data_fun_ast/1
        , bidi_class_fun_ast/0
        , bidi_mirrored_data_fun_ast/1
        , bidi_mirrored_range_data_fun_ast/1
        , bidi_mirrored_fun_ast/0
        , lowercase_mapping_funs_ast/1
        , uppercase_mapping_funs_ast/1
        , titlecase_mapping_funs_ast/1
        , numeric_funs_ast/2
        , nfd_quick_check_fun_ast/1
        , nfc_quick_check_fun_ast/1
        , nfkd_quick_check_fun_ast/1
        , nfkc_quick_check_fun_ast/1
        , block_fun_ast/1
        , codepoint_range_fun_ast/1
        , grapheme_break_fun_ast/0
        , grapheme_break_classes_fun_ast/2
        , word_break_fun_ast/0
        , word_break_classes_fun_ast/2
        , sentence_break_fun_ast/0
        , sentence_break_classes_fun_ast/2
        , line_break_fun_ast/0
        , line_break_classes_fun_ast/2
        ]).

-include_lib("syntax_tools/include/merl.hrl").


index_fun_ast(Name, Ranges) ->
    AST = index_fun_ast_1(split(index(Ranges))),
    ?Q("'@Name@'(CP) -> _@AST.").

index_fun_ast_1({[], []}) ->
    ?Q("undefined");

index_fun_ast_1({[{0, 0, To}], []}) ->
    ?Q(["if"
       ,"  CP =< _@To@ -> CP;"
       ,"  true        -> undefined"
       ,"end"]);

index_fun_ast_1({[{Idx, Id, Id}], []}) ->
    Offset = Id - Idx,
    ?Q(["if"
       ,"  CP == _@Id@ -> CP - _@Offset@;"
       ,"  true        -> undefined"
       ,"end"]);

index_fun_ast_1({[{Idx, From, To}], []}) ->
    Offset = From - Idx,
    ?Q(["if"
       ,"  CP >= _@From@, CP =< _@To@ -> CP - _@Offset@;"
       ,"  true                       -> undefined"
       ,"end"]);

index_fun_ast_1({L1, [{_, From, _}|_]=L2}) ->
    AST1 = index_fun_ast_1(split(L1)),
    AST2 = index_fun_ast_1(split(L2)),
    ?Q(["if"
       ,"  CP < _@From@ ->"
       ,"    _@AST1;"
       ,"  true ->"
       ,"    _@AST2"
       ,"end"]).


range_fun_ast(Name, RangeValues) ->
    range_fun_ast(Name, RangeValues, ?Q("undefined")).

range_fun_ast(Name, RangeValues, DefaultAST) ->
    AST = range_fun_ast_1(split(RangeValues), DefaultAST),
    ?Q("'@Name@'(CP) -> _@AST.").

range_fun_ast_1({[], []}, DefaultAST) ->
    DefaultAST;

range_fun_ast_1({[{0, To, V}], []}, DefaultAST) ->
    ?Q(["if"
       ,"  CP =< _@To@ -> _@V@;"
       ,"  true        -> _@DefaultAST"
       ,"end"]);

range_fun_ast_1({[{Id, Id, V}], []}, DefaultAST) ->
    ?Q(["if"
       ,"  CP == _@Id@ -> _@V@;"
       ,"  true        -> _@DefaultAST"
       ,"end"]);

range_fun_ast_1({[{From, To, V}], []}, DefaultAST) ->
    ?Q(["if"
       ,"  CP >= _@From@, CP =< _@To@ -> _@V@;"
       ,"  true                       -> _@DefaultAST"
       ,"end"]);

range_fun_ast_1({L1, [{From, _, _}|_]=L2}, DefaultAST) ->
    AST1 = range_fun_ast_1(split(L1), DefaultAST),
    AST2 = range_fun_ast_1(split(L2), DefaultAST),
    ?Q(["if"
       ,"  CP < _@From@ ->"
       ,"    _@AST1;"
       ,"  true ->"
       ,"    _@AST2"
       ,"end"]).


data_fun_ast(Name, Items) ->
    data_fun_ast(Name, Items, fun (V) -> V end).

data_fun_ast(Name, Items, ResultASTFun) ->
    ResultAST = ResultASTFun(?Q("V")),
    {Data, BitsSize} = concat_bits(Items),
    ByteSize = round_to_bytes(BitsSize),
    ByteSize1 = ByteSize + 1,
    RemBits = BitsSize rem 8,
    ?Q(["'@Name@'(Index) ->"
       ,"    BitIdx = Index * _@BitsSize@,"
       ,"    Pos = BitIdx div 8,"
       ,"    Offset = BitIdx - (Pos * 8),"
       ,"    Len = if Offset + _@RemBits@ > 8 -> _@ByteSize1@;"
       ,"             true                    -> _@ByteSize@"
       ,"          end,"
       ,"    case binary:part(_@Data@, Pos, Len) of"
       ,"        <<_:Offset,V:_@BitsSize@,_/bits>> -> _@ResultAST"
       ,"    end."
       ]).


common_properties_index_fun_ast(CommonProperties) ->
    index_fun_ast(ucd_properties_idx, compact(CommonProperties)).


category_data_fun_ast(CommonProperties) ->
    Categories = ucd_properties:categories(),
    Names = [ucd_properties:category_name(C) || C <- Categories],
    DecodeASTFun = fun (V) -> decode_value_case_ast(V, Names) end,
    Data = [element(3,V) || V <- CommonProperties],
    Bits = encode_to_bits(Categories, Data),
    data_fun_ast(ucd_category_data, Bits, DecodeASTFun).


category_range_data_fun_ast(Ranges) ->
    RangeValues = [{F,T,ucd_properties:category_name(C)}
                   || {{F,T}, _,C,_,_,_,_,_,_,_,_} <- Ranges],
    Default = ucd_properties:category_name('Cn'),
    range_fun_ast(ucd_category_range_data, RangeValues, ?Q("_@Default@")).


category_fun_ast() ->
    ?Q(["ucd_category(CP) ->"
       ,"  case ucd_properties_idx(CP) of"
       ,"    undefined -> ucd_category_range_data(CP);"
       ,"    Idx       -> ucd_category_data(Idx)"
       ,"  end."]).


is_category_fun_ast(Name, Data, Categories) ->
    Data1 = [element(1,V) || V <- Data, lists:member(element(3,V), Categories)],
    Ranges = [{F, T, true} || {F, T} <- compact_ranges(Data1)],
    range_fun_ast(Name, Ranges, ?Q("false")).


has_property_fun_ast(Name, Properties, Property) ->
    Data1 = [V || {V, Prop} <- Properties, Prop == Property],
    Ranges = [{F, T, true} || {F, T} <- compact_ranges(Data1)],
    range_fun_ast(Name, Ranges, ?Q("false")).


combining_class_data_fun_ast(CommonProperties) ->
    Data = [element(4,V) || V <- CommonProperties],
    data_fun_ast(ucd_combining_class_data, [<<V>> || V <- Data]).


combining_class_range_data_fun_ast(Ranges) ->
    RangeValues = [{F,T,CC} || {{F,T}, _,_,CC,_,_,_,_,_,_,_} <- Ranges],
    range_fun_ast(ucd_combining_class_range_data, RangeValues, ?Q("0")).


combining_class_fun_ast() ->
    ?Q(["ucd_combining_class(CP) ->"
       ,"  case ucd_properties_idx(CP) of"
       ,"    undefined -> ucd_combining_class_range_data(CP);"
       ,"    Idx       -> ucd_combining_class_data(Idx)"
       ,"  end."]).


bidi_class_data_fun_ast(CommonProperties) ->
    Classes = ucd_properties:bidi_classes(),
    Names = [ucd_properties:bidi_class_name(C) || C <- Classes],
    DecodeASTFun = fun (V) -> decode_value_case_ast(V, Names) end,
    Data = [element(5,V) || V <- CommonProperties],
    Bits = encode_to_bits(Classes, Data),
    data_fun_ast(ucd_bidi_class_data, Bits, DecodeASTFun).


bidi_class_range_data_fun_ast(Ranges) ->
    RangeValues = [{F,T,ucd_properties:bidi_class_name(C)}
                   || {{F,T}, _,_,_,C,_,_,_,_,_,_} <- Ranges],
    range_fun_ast(ucd_bidi_class_range_data, RangeValues).


bidi_class_fun_ast() ->
    RangeValues = [{F,T,ucd_properties:bidi_class_name(C)}
                   || {F,T,C} <- ucd_properties:bidi_class_defaults()],
    Default = ucd_properties:bidi_class_name('L'),
    DefaultAST = range_fun_ast_1(split(RangeValues), ?Q("_@Default@")),
    ?Q(["ucd_bidi_class(CP) ->"
       ,"  case ucd_properties_idx(CP) of"
       ,"    undefined ->"
       ,"        case ucd_bidi_class_range_data(CP) of"
       ,"            undefined ->"
       ,"                _@DefaultAST;"
       ,"            V -> V"
       ,"        end;"
       ,"    Idx       -> ucd_bidi_class_data(Idx)"
       ,"  end."]).


bidi_mirrored_data_fun_ast(CommonProperties) ->
    Values = [false, true],
    DecodeASTFun = fun (V) -> decode_value_case_ast(V, Values) end,
    Data = [element(6,V) || V <- CommonProperties],
    Bits = encode_to_bits(Values, Data),
    data_fun_ast(ucd_bidi_mirrored_data, Bits, DecodeASTFun).


bidi_mirrored_range_data_fun_ast(Ranges) ->
    RangeValues = [{F,T,V}
                   || {{F,T}, _,_,_,_,_,_,V,_,_,_} <- Ranges],
    range_fun_ast(ucd_bidi_mirrored_range_data, RangeValues, ?Q("false")).


bidi_mirrored_fun_ast() ->
    ?Q(["ucd_bidi_mirrored(CP) ->"
       ,"  case ucd_properties_idx(CP) of"
       ,"    undefined -> ucd_bidi_mirrored_range_data(CP);"
       ,"    Idx       -> ucd_bidi_mirrored_data(Idx)"
       ,"  end."]).


lowercase_mapping_funs_ast(Data) ->
    case_mapping_funs_ast(ucd_properties:lowercase_mapping(Data)
                         ,ucd_lowercase_mapping
                         ,ucd_lowercase_mapping_idx
                         ,ucd_lowercase_mapping_data).


uppercase_mapping_funs_ast(Data) ->
    case_mapping_funs_ast(ucd_properties:uppercase_mapping(Data)
                         ,ucd_uppercase_mapping
                         ,ucd_uppercase_mapping_idx
                         ,ucd_uppercase_mapping_data).

titlecase_mapping_funs_ast(Data) ->
    case_mapping_funs_ast(ucd_properties:titlecase_mapping(Data)
                         ,ucd_titlecase_mapping
                         ,ucd_titlecase_mapping_idx
                         ,ucd_titlecase_mapping_data).


case_mapping_funs_ast(MappingProperties, Name, IdxName, DataName) ->
    [case_mapping_index_fun_ast(IdxName, MappingProperties)
    ,case_mapping_data_fun_ast(DataName, MappingProperties)
    ,case_mapping_fun_ast(Name, IdxName, DataName)].


case_mapping_index_fun_ast(Name, MappingProperties) ->
    index_fun_ast(Name, compact(MappingProperties)).


case_mapping_data_fun_ast(Name, MappingProperties) ->
    Data = [V || {_,V} <- MappingProperties],
    Size = required_bits(lists:max(Data)),
    data_fun_ast(Name, [<<V:Size>> || V <- Data]).


case_mapping_fun_ast(Name, IdxName, DataName) ->
    ?Q(["'@Name@'(CP) ->"
       ,"  case '@IdxName@'(CP) of"
       ,"    undefined -> undefined;"
       ,"    Idx       -> '@DataName@'(Idx)"
       ,"  end."]).


numeric_funs_ast(Data, ExtraValues) ->
    NumericProperties = ucd_properties:numeric(Data),
    [numeric_index_fun_ast(NumericProperties)
    ,numeric_data_fun_ast(NumericProperties)
    ,numeric_fun_ast(ExtraValues)
    ].

numeric_index_fun_ast(NumericProperties) ->
    index_fun_ast(ucd_numeric_idx, compact(NumericProperties)).


numeric_data_fun_ast(NumericProperties) ->
    Numerics = [V || {_,V} <- NumericProperties],
    Data = list_to_tuple(Numerics),
    ?Q(["ucd_numeric_data(Index) ->"
        "    element(Index + 1, _@Data@)."
       ]).

numeric_fun_ast(ExtraValues) ->
    DefaultAST = binary_search_ast(ExtraValues, ?Q("undefined")),
    ?Q(["ucd_numeric(CP) ->"
       ," case ucd_numeric_idx(CP) of"
       ,"    undefined ->"
       ,"       _@DefaultAST;"
       ,"    Idx ->"
       ,"        ucd_numeric_data(Idx)"
       ," end."
       ]).


nfd_quick_check_fun_ast(NormalizationProperties) ->
    Data = ucd_normalization:nfd_quick_check_no(NormalizationProperties),
    normalization_quickcheck_fun_ast(ucd_nfd_quickcheck, [{V,no} || V <- Data]).


nfc_quick_check_fun_ast(NormalizationProperties) ->
    Data = ucd_normalization:nfc_quick_check(NormalizationProperties),
    normalization_quickcheck_fun_ast(ucd_nfc_quickcheck, Data).


nfkd_quick_check_fun_ast(NormalizationProperties) ->
    Data = ucd_normalization:nfkd_quick_check_no(NormalizationProperties),
    normalization_quickcheck_fun_ast(ucd_nfkd_quickcheck, [{V,no} || V <- Data]).


nfkc_quick_check_fun_ast(NormalizationProperties) ->
    Data = ucd_normalization:nfkc_quick_check(NormalizationProperties),
    normalization_quickcheck_fun_ast(ucd_nfkc_quickcheck, Data).


normalization_quickcheck_fun_ast(Name, Data) ->
    Rs = [case V of
              {{F,T},R} -> {F,T,R};
              {Cp, R}   -> {Cp, Cp, R}
          end || V <- Data],
    range_fun_ast(Name, Rs, ?Q("yes")).


block_fun_ast(Blocks) ->
    RangeValues = [{F,T,V} || {{F,T}, V} <- Blocks],
    range_fun_ast(ucd_block, RangeValues, ?Q("no_block")).


codepoint_range_fun_ast(Ranges) ->
    RangeValues = [codepoint_range_value(R) || R <- Ranges],
    range_fun_ast(ucd_range, RangeValues).

codepoint_range_value(Range) ->
    {From, To} = element(1, Range),
    Name = ucd_properties:range_name(element(2, Range)),
    {From, To, Name}.


grapheme_break_fun_ast() ->
    Data = ucd_segmentation:grapheme_breaks(),
    segmentation_fun_ast(ucd_grapheme_break, Data, other).


word_break_fun_ast() ->
    Data = ucd_segmentation:word_breaks(),
    segmentation_fun_ast(ucd_word_break, Data, other).


sentence_break_fun_ast() ->
    Data = ucd_segmentation:sentence_breaks(),
    segmentation_fun_ast(ucd_sentence_break, Data, other).


line_break_fun_ast() ->
    Data = ucd_segmentation:line_breaks(),
    segmentation_fun_ast(ucd_line_break, Data, xx).


segmentation_fun_ast(Name, Data, Default) ->
    RangeValues = [case V of
                       {{F,T},R} -> {F,T,R};
                       {Cp, R}   -> {Cp, Cp, R}
                   end || V <- Data],
    range_fun_ast(Name, RangeValues, ?Q("_@Default@")).


grapheme_break_classes_fun_ast(Name, Classes) ->
    Data = ucd_segmentation:grapheme_breaks(),
    segmentation_classes_fun_ast(Name, Data, Classes).


word_break_classes_fun_ast(Name, Classes) ->
    Data = ucd_segmentation:word_breaks(),
    segmentation_classes_fun_ast(Name, Data, Classes).


sentence_break_classes_fun_ast(Name, Classes) ->
    Data = ucd_segmentation:sentence_breaks(),
    segmentation_classes_fun_ast(Name, Data, Classes).


line_break_classes_fun_ast(Name, Classes) ->
    Data = ucd_segmentation:line_breaks(),
    segmentation_classes_fun_ast(Name, Data, Classes).


segmentation_classes_fun_ast(Name, Data, Classes) ->
    Data1 = [V || {V, Class} <- Data, lists:member(Class, Classes)],
    Ranges = [{F, T, true} || {F, T} <- compact_ranges(Data1)],
    range_fun_ast(Name, Ranges, ?Q("false")).


decode_value_case_ast(ValueAST, Values) ->
    Cases = [?Q("_@V@ -> _@R@") || {R,V} <- enum_values(Values)],
    ?Q(["case _@ValueAST of"
       ," _ -> _@_@Cases"
       ,"end"
       ]).


binary_search_ast(Values, DefaultAST) ->
    binary_search_ast_1(split(Values), DefaultAST).

binary_search_ast_1({[], []}, DefaultAST) ->
    DefaultAST;

binary_search_ast_1({[{K, V}], []}, DefaultAST) ->
    ?Q(["if"
       ,"  CP == _@K@ -> _@V@;"
       ,"  true       -> _@DefaultAST"
       ,"end"]);

binary_search_ast_1({L1, [{K, V}|_]=L2}, DefaultAST) ->
    AST1 = binary_search_ast_1(split(L1), DefaultAST),
    AST2 = binary_search_ast_1(split(L2), DefaultAST),
    ?Q(["if"
       ,"  CP == _@K@ ->"
       ,"    _@V@;"
       ,"  CP < _@K@ ->"
       ,"    _@AST1;"
       ,"  true ->"
       ,"    _@AST2"
       ,"end"]).


index(Ranges) -> index(Ranges, 0, []).

index([], _, Acc) ->
    lists:reverse(Acc);

index([{First,Last} | T], Idx, Acc) ->
    index(T, Idx + Last - First + 1, [{Idx, First, Last} | Acc]).


split(L) -> split([], L, L).

split(L1, L2, []) ->
    {lists:reverse(L1), L2};

split(L1, [H|T], [_]) ->
    {lists:reverse([H | L1]), T};

split(L1, [H|T], [_,_|R]) ->
    split([H | L1], T, R).


encode_to_bits(Types, Data) ->
    Map = maps:from_list(enum_values(Types)),
    BitsSize = required_bits(maps:size(Map)),
    [begin V = maps:get(T,Map), <<V:BitsSize>> end || T <- Data].


concat_bits([V|_]=Vs) ->
    {BitsData, N} = lists:foldr(fun(B1,{B0,C}) ->
                                   {<<B1/bitstring,B0/bitstring>>, C+1}
                                end, {<<>>, 0}, Vs),
    BitsSize = bit_size(V),
    BytesData = case (BitsSize * N) rem 8 of
                    0   -> BitsData;
                    Rem -> <<BitsData/bitstring, 0:(8-Rem)>>
                end,
    {BytesData, BitsSize}.


required_bits(N) when is_integer(N), N > 1 ->
    Bits = math:log2(N),
    T = trunc(Bits),
    case Bits - T == 0 of
        true  -> T;
        false -> T + 1
    end.


round_to_bytes(Bits) ->
    case Bits rem 8 of
        0 -> (Bits div 8);
        _ -> (Bits div 8) + 1
    end.


enum_values(Values) ->
    Size = length(Values),
    lists:zip(Values,lists:seq(0,Size-1)).


compact(Data) ->
    compact_ranges([element(1,V) || V <- Data]).

compact_ranges([]) -> [];
compact_ranges([H|T]) ->
    case H of
        {Cp1, Cp2} -> compact_ranges(T, [{Cp1, Cp2}]);
        Cp         -> compact_ranges(T, [{Cp, Cp}])
    end.

compact_ranges([], Acc) ->
    lists:reverse(Acc);

compact_ranges([H|T], [{Cp1, Cp2}=Range | Acc]) ->
    case H of
        {NCp1, NCp2} ->
            compact_ranges(T, [{NCp1, NCp2}, Range | Acc]);
        Cp when Cp == Cp2 + 1 ->
            compact_ranges(T, [{Cp1, Cp} | Acc]);
        Cp ->
            compact_ranges(T, [{Cp, Cp}, Range | Acc])
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

index_test_() -> [
  ?_assertEqual([], index([]))
 ,?_assertEqual([ {0, 10, 20}
                , {11, 30, 40}
                , {22, 50, 60}]
               , index([{10,20}, {30,40}, {50,60}]))
].

split_test_() -> [
  ?_assertEqual({[], []}, split([]))
 ,?_assertEqual({[a], []}, split([a]))
 ,?_assertEqual({[a], [b]}, split([a,b]))
 ,?_assertEqual({[a,b], [c,d]}, split([a,b,c,d]))
 ,?_assertEqual({[a,b,c], [d,e]}, split([a,b,c,d,e]))
].

encode_to_bits_test_() -> [
  ?_assertEqual([<<0:3>>, <<1:3>>, <<0:3>>, <<3:3>>, <<2:3>>],
                encode_to_bits([a,b,c,d,e,f], [a, b, a, d, c]))
].

concat_bits_test_() -> [
  ?_assertEqual({<<1:5,2:5,0:6>>, 5}, concat_bits([<<1:5>>,<<2:5>>]))
].

required_bits_test_() -> [
  ?_assertEqual(1, required_bits(2))
 ,?_assertEqual(5, required_bits(31))
 ,?_assertEqual(5, required_bits(32))
 ,?_assertEqual(16, required_bits(40000))
].

round_to_bytes_test_() -> [
  ?_assertEqual(1, round_to_bytes(1))
 ,?_assertEqual(1, round_to_bytes(8))
 ,?_assertEqual(2, round_to_bytes(9))
].

compact_test_() -> [
  ?_assertEqual([{0, 2}
                ,{4, 10}
                ,{15, 15}]
               ,compact([{0, a}
                        ,{1, b}
                        ,{2, c}
                        ,{{4,10}, d}
                        ,{15, e}]))
].

index_fun_ast_test_() ->
  Funs = [
    ucd_codegen:index_fun_ast(idx1,[{0,1}, {10,10}, {15,20}])
  ],
  {setup, fun() -> test_mod(ucd_index_funs, Funs) end, fun code:purge/1, [
     ?_assertEqual(0, ucd_index_funs:idx1(0))
    ,?_assertEqual(1, ucd_index_funs:idx1(1))
    ,?_assertEqual(2, ucd_index_funs:idx1(10))
    ,?_assertEqual(3, ucd_index_funs:idx1(15))
    ,?_assertEqual(8, ucd_index_funs:idx1(20))
    ,?_assertEqual(undefined, ucd_index_funs:idx1(5))
    ,?_assertEqual(undefined, ucd_index_funs:idx1(25))
  ]}.

range_fun_ast_test_() ->
  Funs = [
    ucd_codegen:range_fun_ast(range1,[{0,1,a}, {10,10,b}, {15,20,c}])
   ,ucd_codegen:range_fun_ast(range2,[{15,20,c}], ?Q("default"))
  ],
  {setup, fun() -> test_mod(ucd_range_funs, Funs) end, fun code:purge/1, [
     ?_assertEqual(a, ucd_range_funs:range1(0))
    ,?_assertEqual(a, ucd_range_funs:range1(1))
    ,?_assertEqual(b, ucd_range_funs:range1(10))
    ,?_assertEqual(c, ucd_range_funs:range1(15))
    ,?_assertEqual(c, ucd_range_funs:range1(20))
    ,?_assertEqual(undefined, ucd_range_funs:range1(5))
    ,?_assertEqual(undefined, ucd_range_funs:range1(25))

    ,?_assertEqual(c, ucd_range_funs:range2(18))
    ,?_assertEqual(default, ucd_range_funs:range2(5))
    ,?_assertEqual(default, ucd_range_funs:range2(25))
  ]}.

data_fun_ast_test_() ->
  Funs = [
    ucd_codegen:data_fun_ast(f5,[<<1:5>>, <<2:5>>, <<3:5>>, <<4:5>>])
   ,ucd_codegen:data_fun_ast(f8,[<<1:8>>, <<2:8>>, <<3:8>>, <<4:8>>])
   ,ucd_codegen:data_fun_ast(f14,[<<1:14>>, <<2:14>>, <<3:14>>, <<4:14>>])
   ,ucd_codegen:data_fun_ast(fv,[<<1:5>>, <<2:5>>, <<0:5>>],
                             fun (V) -> decode_value_case_ast(V, [a,b,c,d]) end)
  ],
  {setup, fun() -> test_mod(ucd_data_funs, Funs) end, fun code:purge/1, [
     ?_assertEqual(1, ucd_data_funs:f5(0))
    ,?_assertEqual(2, ucd_data_funs:f5(1))
    ,?_assertEqual(4, ucd_data_funs:f5(3))

    ,?_assertEqual(1, ucd_data_funs:f8(0))
    ,?_assertEqual(2, ucd_data_funs:f8(1))
    ,?_assertEqual(4, ucd_data_funs:f8(3))

    ,?_assertEqual(1, ucd_data_funs:f14(0))
    ,?_assertEqual(2, ucd_data_funs:f14(1))
    ,?_assertEqual(4, ucd_data_funs:f14(3))

    ,?_assertEqual(b, ucd_data_funs:fv(0))
    ,?_assertEqual(c, ucd_data_funs:fv(1))
    ,?_assertEqual(a, ucd_data_funs:fv(2))
  ]}.

binary_search_ast_test_() ->
  S1 = binary_search_ast([{0,a}, {10,b}, {15,c}], ?Q("default")),
  Funs = [
    ?Q("f1(CP) -> _@S1.")
  ],
  {setup, fun() -> test_mod(ucd_binary_search, Funs) end, fun code:purge/1, [
     ?_assertEqual(a, ucd_binary_search:f1(0))
    ,?_assertEqual(b, ucd_binary_search:f1(10))
    ,?_assertEqual(c, ucd_binary_search:f1(15))
    ,?_assertEqual(default, ucd_binary_search:f1(5))
    ,?_assertEqual(default, ucd_binary_search:f1(25))
  ]}.


test_mod(Mod, Funs) ->
    Forms = [?Q("-module('@Mod@')."),
             ?Q("-compile(export_all).")
             | Funs
            ],
    merl:compile_and_load(Forms),
    Mod.

-endif.
