-module(ucd_codegen).
-export([ index_fun_ast/2
        , data_fun_ast/2
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


data_fun_ast(Name, Items) ->
    {Data, BitsSize} = concat_bits(Items),
    ByteSize = round_to_bytes(BitsSize),
    ByteSize1 = ByteSize + 1,
    ?Q(["'@Name@'(Index) ->"
       ,"    BitIdx = Index * _@BitsSize@,"
       ,"    Pos = BitIdx div 8,"
       ,"    Offset = BitIdx - (Pos * 8),"
       ,"    Len = if ((Offset + _@BitsSize@) rem 8) == 0 -> _@ByteSize@;"
       ,"             true                                -> _@ByteSize1@"
       ,"          end,"
       ,"    case binary:part(_@Data@, Pos, Len) of"
       ,"        <<_:Offset,V:_@BitsSize@,_/bits>> -> V"
       ,"    end."
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


round_to_bytes(Bits) ->
    case Bits rem 8 of
        0 -> (Bits div 8);
        _ -> (Bits div 8) + 1
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

concat_bits_test_() -> [
  ?_assertEqual({<<1:5,2:5,0:6>>, 5}, concat_bits([<<1:5>>,<<2:5>>]))
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

data_fun_ast_test_() ->
  Funs = [
    ucd_codegen:data_fun_ast(f5,[<<1:5>>, <<2:5>>, <<3:5>>, <<4:5>>])
   ,ucd_codegen:data_fun_ast(f8,[<<1:8>>, <<2:8>>, <<3:8>>, <<4:8>>])
   ,ucd_codegen:data_fun_ast(f14,[<<1:14>>, <<2:14>>, <<3:14>>, <<4:14>>])
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
  ]}.


test_mod(Mod, Funs) ->
    Forms = [?Q("-module('@Mod@')."),
             ?Q("-compile(export_all).")
             | Funs
            ],
    merl:compile_and_load(Forms),
    Mod.

-endif.
