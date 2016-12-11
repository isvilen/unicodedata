-module(ucd_names).
-export([ blocks/0
        , aliases/0
        , aliases/2
        , aliases_types/0
        , sequences/0
        ]).


blocks() ->
    Data = ucd:fold_lines(fun (L, Acc) -> [block_data(L) | Acc] end
                         ,"Blocks.txt"
                         ,[]),
    lists:reverse(Data).


aliases() ->
    Data = ucd:fold_lines(fun (L, Acc) -> add_alias(L, Acc) end
                         ,"NameAliases.txt"
                         ,#{}),
    Data1 = [{K, lists:reverse(Vs)} || {K, Vs} <- maps:to_list(Data)],
    lists:sort(fun ({C1,_}, {C2,_}) -> C1 =< C2 end, Data1).


aliases(Aliases, Type) ->
    lists:filtermap(fun({Cp, Vs}) -> aliases(Cp, Type, Vs)  end, Aliases).

aliases(Cp, Type, Vs) ->
    case [V || {T, V} <- Vs, T == Type] of
        []  -> false;
        Vs1 -> {true, {Cp, Vs1}}
    end.



aliases_types() -> [correction, control, alternate, figment, abbreviation].


sequences() ->
    Data = ucd:fold_lines(fun (L, Acc) -> [sequence_data(L) | Acc] end
                         ,"NamedSequences.txt"
                         ,[]),
    lists:reverse(Data).


block_data([CpOrRange, Name]) -> {ucd:codepoint_or_range(CpOrRange), Name}.


add_alias(L, Acc) ->
    {K, V} = alias_data(L),
    maps:update_with(K, fun (Vs) -> [V | Vs] end, [V], Acc).

alias_data([Cp,Name,Type]) ->
    {ucd:codepoint(Cp), {alias_type(Type), Name}}.

alias_type(<<"correction">>)   -> correction;
alias_type(<<"control">>)      -> control;
alias_type(<<"alternate">>)    -> alternate;
alias_type(<<"figment">>)      -> figment;
alias_type(<<"abbreviation">>) -> abbreviation.


sequence_data([Name, Cps]) ->
    {Name, [ucd:codepoint(Cp) || Cp <- binary:split(Cps, <<" ">>, [global])]}.
