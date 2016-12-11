-module(ucd_unihan).
-export([numeric_data/0
        ,numeric_types/0
        ]).


numeric_data() ->
    Data = ucd:fold_lines(fun (L, Acc) -> [numeric_data(L) | Acc] end
                         ,"Unihan_NumericValues.txt"
                         ,[]),
    lists:sort(fun ({C1,_}, {C2,_}) -> C1 =< C2 end, Data).


numeric_types() -> [k_accounting_numeric , k_other_numeric, k_primary_numeric].


numeric_data([C,T,N]) -> {ucd:codepoint(C), {numeric_type(T), to_integer(N)}}.


numeric_type(<<"kAccountingNumeric">>) -> k_accounting_numeric;
numeric_type(<<"kOtherNumeric">>)      -> k_other_numeric;
numeric_type(<<"kPrimaryNumeric">>)    -> k_primary_numeric.


to_integer(V) -> list_to_integer(binary_to_list(V)).
