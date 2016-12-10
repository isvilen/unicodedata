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
        , combining_class_data_fun_ast/1
        , combining_class_range_data_fun_ast/1
        , combining_class_fun_ast/0
        , bidi_class_data_fun_ast/1
        , bidi_class_range_data_fun_ast/1
        , bidi_class_fun_ast/0
        , lowercase_mapping_funs_ast/1
        , uppercase_mapping_funs_ast/1
        , titlecase_mapping_funs_ast/1
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
    Ranges = [{From,To} || {From,To,_} <- CommonProperties],
    index_fun_ast(ucd_properties_idx, Ranges).


category_data_fun_ast(CommonProperties) ->
    Categories = ucd_properties:categories(),
    Names = [ucd_properties:category_name(C) || C <- Categories],
    DecodeASTFun = fun (V) -> decode_value_case_ast(V, Names) end,
    Data = lists:flatmap(fun ({_, _, Vs}) -> [element(3,V) || V <- Vs] end,
                         CommonProperties),
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


combining_class_data_fun_ast(CommonProperties) ->
    Data = lists:flatmap(fun ({_, _, Vs}) -> [element(4,V) || V <- Vs] end,
                         CommonProperties),
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
    Data = lists:flatmap(fun ({_, _, Vs}) -> [element(5,V) || V <- Vs] end,
                         CommonProperties),
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


lowercase_mapping_funs_ast(Data) ->
    case_mapping_funs_ast(Data
                         ,lowercase_mapping
                         ,ucd_lowercase_mapping
                         ,ucd_lowercase_mapping_idx
                         ,ucd_lowercase_mapping_data).


uppercase_mapping_funs_ast(Data) ->
    case_mapping_funs_ast(Data
                         ,uppercase_mapping
                         ,ucd_uppercase_mapping
                         ,ucd_uppercase_mapping_idx
                         ,ucd_uppercase_mapping_data).

titlecase_mapping_funs_ast(Data) ->
    case_mapping_funs_ast(Data
                         ,titlecase_mapping
                         ,ucd_titlecase_mapping
                         ,ucd_titlecase_mapping_idx
                         ,ucd_titlecase_mapping_data).


case_mapping_funs_ast(Data, Mapping, Name, IdxName, DataName) ->
    MappingProperties = ucd_properties:compact(Mapping, Data),
    [case_mapping_index_fun_ast(IdxName, MappingProperties)
    ,case_mapping_data_fun_ast(DataName, MappingProperties)
    ,case_mapping_fun_ast(Name, IdxName, DataName)].


case_mapping_index_fun_ast(Name, MappingProperties) ->
    Ranges = [{From,To} || {From,To,_} <- MappingProperties],
    index_fun_ast(Name, Ranges).


case_mapping_data_fun_ast(Name, MappingProperties) ->
    Data = lists:flatmap(fun ({_, _, Vs}) -> [V || {_,V} <- Vs] end,
                         MappingProperties),
    Size = required_bits(lists:max(Data)),
    data_fun_ast(Name, [<<V:Size>> || V <- Data]).


case_mapping_fun_ast(Name, IdxName, DataName) ->
    ?Q(["'@Name@'(CP) ->"
       ,"  case '@IdxName@'(CP) of"
       ,"    undefined -> undefined;"
       ,"    Idx       -> '@DataName@'(Idx)"
       ,"  end."]).


decode_value_case_ast(ValueAST, Values) ->
    Cases = [?Q("_@V@ -> _@R@") || {R,V} <- enum_values(Values)],
    ?Q(["case _@ValueAST of"
       ," _ -> _@_@Cases"
       ,"end"
       ]).


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


test_mod(Mod, Funs) ->
    Forms = [?Q("-module('@Mod@')."),
             ?Q("-compile(export_all).")
             | Funs
            ],
    merl:compile_and_load(Forms),
    Mod.

-endif.
