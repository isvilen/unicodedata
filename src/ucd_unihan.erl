-module(ucd_unihan).
-export([numeric_data/0
        ,numeric_types/0
        ]).


numeric_data() ->
    Data = ucd:fold_lines(fun numeric_data/1, "Unihan_NumericValues.txt"),
    ucd:sort_by_codepoints(Data).


numeric_types() -> [k_accounting_numeric , k_other_numeric, k_primary_numeric].


numeric_data([C,T,N]) ->
    {ucd:codepoint(C), {numeric_type(T), ucd:to_integer(N)}}.


numeric_type(<<"kAccountingNumeric">>) -> k_accounting_numeric;
numeric_type(<<"kOtherNumeric">>)      -> k_other_numeric;
numeric_type(<<"kPrimaryNumeric">>)    -> k_primary_numeric.
