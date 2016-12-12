-module(ucd_normalization).
-export([ composition_exclusions/0
        , normalization_properties/0
        , full_composition_exclusion/1
        , nfd_quick_check_no/1
        , nfc_quick_check_no/1
        , nfc_quick_check_maybe/1
        , nfc_quick_check/1
        , nfkd_quick_check_no/1
        , nfkc_quick_check_no/1
        , nfkc_quick_check_maybe/1
        , nfkc_quick_check/1
        , nfkc_casefold/1
        , changes_when_nfkc_casefolded/1
        ]).


composition_exclusions() ->
    ucd:fold_lines(fun ([CP])-> ucd:codepoint(CP) end
                  ,"CompositionExclusions.txt").


normalization_properties() ->
    Data = ucd:fold_lines(fun normalization_properties_data/2
                         ,"DerivedNormalizationProps.txt", []),
    lists:reverse(Data).


full_composition_exclusion(Properties) ->
    [V || {V, full_composition_exclusion} <- Properties].


nfd_quick_check_no(Properties) ->
    [V || {V, nfd_quick_check_no} <- Properties].


nfc_quick_check_no(Properties) ->
    [V || {V, nfc_quick_check_no} <- Properties].


nfc_quick_check_maybe(Properties) ->
    [V || {V, nfc_quick_check_maybe} <- Properties].


nfc_quick_check(Properties) ->
    Vs = lists:filtermap(fun ({V, nfc_quick_check_no})    -> {true, {V,no}};
                             ({V, nfc_quick_check_maybe}) -> {true, {V,maybe}};
                             (_)                          -> false
                         end, Properties),
    ucd:sort_by_codepoints(Vs).


nfkd_quick_check_no(Properties) ->
    [V || {V, nfkd_quick_check_no} <- Properties].


nfkc_quick_check_no(Properties) ->
    [V || {V, nfkc_quick_check_no} <- Properties].


nfkc_quick_check_maybe(Properties) ->
    [V || {V, nfkc_quick_check_maybe} <- Properties].


nfkc_quick_check(Properties) ->
    Vs = lists:filtermap(fun ({V, nfkc_quick_check_no})    -> {true,{V,no}};
                             ({V, nfkc_quick_check_maybe}) -> {true,{V,maybe}};
                             (_)                           -> false
                         end, Properties),
    ucd:sort_by_codepoints(Vs).


nfkc_casefold(Properties) ->
    [{V, Cps} || {V, nfkc_casefold, Cps} <- Properties].


changes_when_nfkc_casefolded(Properties) ->
    [V || {V, changes_when_nfkc_casefolded} <- Properties].


normalization_properties_data([_,<<"FC_NFKC">>,_], Acc) ->
    Acc;
normalization_properties_data([V,<<"Full_Composition_Exclusion ">>], Acc) ->
    [{ucd:codepoint_or_range(V), full_composition_exclusion} | Acc];

normalization_properties_data([V,<<"NFD_QC">>,<<"N ">>], Acc) ->
    [{ucd:codepoint_or_range(V), nfd_quick_check_no} | Acc];

normalization_properties_data([V,<<"NFC_QC">>,<<"N ">>], Acc) ->
    [{ucd:codepoint_or_range(V), nfc_quick_check_no} | Acc];

normalization_properties_data([V,<<"NFC_QC">>,<<"M ">>], Acc) ->
    [{ucd:codepoint_or_range(V), nfc_quick_check_maybe} | Acc];

normalization_properties_data([V,<<"NFKD_QC">>,<<"N ">>], Acc) ->
    [{ucd:codepoint_or_range(V), nfkd_quick_check_no} | Acc];

normalization_properties_data([V,<<"NFKC_QC">>,<<"N ">>], Acc) ->
    [{ucd:codepoint_or_range(V), nfkc_quick_check_no} | Acc];

normalization_properties_data([V,<<"NFKC_QC">>,<<"M ">>], Acc) ->
    [{ucd:codepoint_or_range(V), nfkc_quick_check_maybe} | Acc];

normalization_properties_data([_, <<"Expands_On_NFD ">>], Acc) ->
    Acc;

normalization_properties_data([_, <<"Expands_On_NFC ">>], Acc) ->
    Acc;

normalization_properties_data([_, <<"Expands_On_NFKD ">>], Acc) ->
    Acc;

normalization_properties_data([_, <<"Expands_On_NFKC ">>], Acc) ->
    Acc;

normalization_properties_data([V,<<"NFKC_CF">>,Cps], Acc) ->
    [{ucd:codepoint_or_range(V), nfkc_casefold, ucd:codepoints(Cps)} | Acc];

normalization_properties_data([V,<<"Changes_When_NFKC_Casefolded ">>], Acc) ->
    [{ucd:codepoint_or_range(V), changes_when_nfkc_casefolded} | Acc].
