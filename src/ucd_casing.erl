-module(ucd_casing).
-export([ case_folding/0
        , special_casing/0
        ]).

case_folding() ->
    ucd:fold_lines(fun case_folding_data/1, "CaseFolding.txt").


special_casing() ->
    ucd:fold_lines(fun special_casing_data/1, "SpecialCasing.txt").


case_folding_data([Cp, <<"C">>, Mapping, <<>>]) ->
    {ucd:codepoint(Cp), common, ucd:codepoint(Mapping)};

case_folding_data([Cp, <<"S">>, Mapping, <<>>]) ->
    {ucd:codepoint(Cp), simple, ucd:codepoint(Mapping)};

case_folding_data([Cp, <<"F">>, Mappings, <<>>]) ->
    {ucd:codepoint(Cp), full, ucd:codepoints(Mappings)};

case_folding_data([Cp, <<"T">>, Mapping, <<>>]) ->
    {ucd:codepoint(Cp), turkic, ucd:codepoint(Mapping)}.


special_casing_data([CP, Lower, Title, Upper, Conditions, <<>>]) ->
    Cs = [casing_context(C) || C <- binary:split(Conditions, <<" ">>, [global])],
    special_casing_data(CP, Lower, Title, Upper, Cs);

special_casing_data([CP, Lower, Title, Upper, <<>>]) ->
    special_casing_data(CP, Lower, Title, Upper, []).


special_casing_data(CP, Lower, Title, Upper, Conditions) ->
    {ucd:codepoint(CP)
    ,ucd:codepoints(Lower), ucd:codepoints(Title), ucd:codepoints(Upper)
    ,Conditions}.


casing_context(<<"Final_Sigma">>)       -> final_sigma;
casing_context(<<"After_Soft_Dotted">>) -> after_soft_dotted;
casing_context(<<"More_Above">>)        -> more_above;
casing_context(<<"Before_Dot">>)        -> before_dot;
casing_context(<<"Not_Before_Dot">>)    -> not_before_dot;
casing_context(<<"After_I">>)           -> after_I;
casing_context(Lang)                    -> Lang.
