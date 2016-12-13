-module(ucd_transform).
-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

parse_transform(Forms, _Opts) ->
    Forms ++ forms(collect_ucd_funs(Forms)).


collect_ucd_funs(Forms) ->
    sets:to_list(lists:foldl(fun collect_ucd_funs/2, sets:new(), Forms)).

collect_ucd_funs(Form, UcdFuns) ->
    erl_syntax_lib:fold(fun collect_ucd_funs_1/2, UcdFuns, Form).

collect_ucd_funs_1(AST, UcdFuns) ->
    case AST of
        ?Q("fun '@Name'/90919") ->
            collect_ucd_funs_2(Name, erl_syntax:concrete(Q1), UcdFuns);
        ?Q("'@Name'(_@@Args)") ->
            case erl_syntax:type(Name) of
                atom -> collect_ucd_funs_2(Name, length(Args), UcdFuns);
                _    -> UcdFuns
            end;
        _ ->
            UcdFuns
    end.

collect_ucd_funs_2(NameAST, Arity, UcdFuns) ->
    case erl_syntax:atom_name(NameAST) of
        "ucd_" ++ _ ->
            Fun = {erl_syntax:atom_value(NameAST), Arity},
            sets:add_element(Fun, UcdFuns);
        _ ->
            UcdFuns
    end.


forms(Funs) ->
    State = #{ data => undefined
             , common_properties => undefined
             , ranges => undefined
             , generated_functions => sets:new()
             },
    {Forms, _} = lists:mapfoldl(fun forms/2, State, Funs),
    erl_syntax:revert_forms(lists:append(Forms)).


forms({ucd_category, 1}, State0) ->
    {Properties, State1} = common_properties_data(State0),
    {Ranges, State2} = ranges_data(State1),
    Forms = [ ucd_codegen:category_data_fun_ast(Properties)
            , ucd_codegen:category_range_data_fun_ast(Ranges)
            , ucd_codegen:category_fun_ast()
            ],
    ensure_common_properties_index(Forms, State2);

forms(_, State) ->
    {[], State}.


ensure_common_properties_index(Forms, State0) ->
    {Properties, State1} = common_properties_data(State0),
    case is_function_generated(ucd_properties_idx, State1) of
        true  ->
            {Forms, State1};
        false ->
            IdxFun = ucd_codegen:common_properties_index_fun_ast(Properties),
            {[IdxFun | Forms], function_generated(ucd_properties_idx, State1)}
    end.


is_function_generated(Name, #{generated_functions := Funs}) ->
    sets:is_element(Name, Funs).


function_generated(Name, #{generated_functions := Funs}=State) ->
    State#{generated_functions := sets:add_element(Name, Funs)}.


ucd_data(#{data := undefined}=State) ->
    Data = ucd_properties:data(),
    {Data, State#{data := Data}};

ucd_data(#{data := Data}=State) ->
    {Data, State}.


common_properties_data(#{common_properties := undefined}=State0) ->
    {Data, State1} = ucd_data(State0),
    Properties = ucd_properties:compact(common_properties, Data),
    {Properties, State1#{common_properties := Properties}};

common_properties_data(#{common_properties := Properties}=State) ->
    {Properties, State}.


ranges_data(#{ranges := undefined}=State0) ->
    {Data, State1} = ucd_data(State0),
    Ranges = ucd_properties:ranges(Data),
    {Ranges, State1#{ranges := Ranges}};

ranges_data(#{ranges := Ranges}=State) ->
    {Ranges, State}.
