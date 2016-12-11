-module(ucd_casing).
-export([ case_folding/0
        , special_casing/0
        ]).

case_folding() ->
    Data = ucd:fold_lines(fun (L, Acc) -> [case_folding_data(L) | Acc] end
                         ,"CaseFolding.txt"
                         ,[]),
    lists:reverse(Data).


special_casing() ->
    Data = ucd:fold_lines(fun (L, Acc) -> [special_casing_data(L) | Acc] end
                         ,"SpecialCasing.txt"
                         ,[]),
    lists:reverse(Data).


case_folding_data([Cp, <<"C">>, Mapping, <<>>]) ->
    {ucd:codepoint(Cp), common, ucd:codepoint(Mapping)};

case_folding_data([Cp, <<"S">>, Mapping, <<>>]) ->
    {ucd:codepoint(Cp), simple, ucd:codepoint(Mapping)};

case_folding_data([Cp, <<"F">>, Mappings, <<>>]) ->
    {ucd:codepoint(Cp), full, codepoints(Mappings)};

case_folding_data([Cp, <<"T">>, Mapping, <<>>]) ->
    {ucd:codepoint(Cp), turkic, ucd:codepoint(Mapping)}.


special_casing_data([CP, Lower, Title, Upper, Conditions, <<>>]) ->
    Cs = [casing_context(C) || C <- binary:split(Conditions, <<" ">>, [global])],
    special_casing_data(CP, Lower, Title, Upper, Cs);

special_casing_data([CP, Lower, Title, Upper, <<>>]) ->
    special_casing_data(CP, Lower, Title, Upper, []).


special_casing_data(CP, Lower, Title, Upper, Conditions) ->
    {ucd:codepoint(CP)
    ,codepoints(Lower), codepoints(Title), codepoints(Upper)
    ,Conditions}.


codepoints(<<>>) ->
    [];
codepoints(Cps) ->
    [ucd:codepoint(Cp) || Cp <- binary:split(Cps, <<" ">>, [global])].


casing_context(<<"Final_Sigma">>)       -> final_sigma;
casing_context(<<"After_Soft_Dotted">>) -> after_soft_dotted;
casing_context(<<"More_Above">>)        -> more_above;
casing_context(<<"Before_Dot">>)        -> before_dot;
casing_context(<<"Not_Before_Dot">>)    -> not_before_dot;
casing_context(<<"After_I">>)           -> after_I;
casing_context(Lang)                    -> Lang.
