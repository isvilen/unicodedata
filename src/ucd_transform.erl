-module(ucd_transform).
-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

parse_transform(Forms, _Opts) ->
    {Forms1, UcdFuns} = collect_ucd_funs(Forms),
    Forms1 ++ forms(UcdFuns).


collect_ucd_funs(Forms) ->
    {Forms1, UcdFuns} = lists:mapfoldr(fun collect_ucd_funs/2, sets:new(), Forms),
    {erl_syntax:revert_forms(Forms1), sets:to_list(UcdFuns)}.

collect_ucd_funs(Form, UcdFuns) ->
    erl_syntax_lib:mapfold(fun collect_ucd_funs_1/2, UcdFuns, Form).

collect_ucd_funs_1(AST, UcdFuns) ->
    case AST of
        ?Q("fun '@Name'/90919") ->
            {AST, collect_ucd_funs_2(Name, erl_syntax:concrete(Q1), UcdFuns)};
        ?Q("ucd_is_category(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                     ucd_is_category,
                                     fun ucd_properties:categories/0);
        ?Q("ucd_has_property(_@CP, _@V)") ->
            collect_ucd_property_fun(AST, CP, V, UcdFuns,
                                     ucd_has_property,
                                     fun ucd_properties:properties_list_types/0);
        ?Q("ucd_grapheme_break(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                    ucd_grapheme_break,
                                    fun ucd_segmentation:grapheme_break_classes/0);
        ?Q("ucd_word_break(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                    ucd_word_break,
                                    fun ucd_segmentation:word_break_classes/0);
        ?Q("ucd_sentence_break(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                    ucd_sentence_break,
                                    fun ucd_segmentation:sentence_break_classes/0);
        ?Q("ucd_line_break(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                    ucd_line_break,
                                    fun ucd_segmentation:line_break_classes/0);
        ?Q("ucd_special_casing(_@CP, lower)") ->
            collect_ucd_funs_replace({ucd_special_casing_lower, 1}, CP, UcdFuns);
        ?Q("ucd_special_casing(_@CP, title)") ->
            collect_ucd_funs_replace({ucd_special_casing_title, 1}, CP, UcdFuns);
        ?Q("ucd_special_casing(_@CP, upper)") ->
            collect_ucd_funs_replace({ucd_special_casing_upper, 1}, CP, UcdFuns);
        ?Q("'@Name'(_@@Args)") ->
            case erl_syntax:type(Name) of
                atom -> {AST, collect_ucd_funs_2(Name, length(Args), UcdFuns)};
                _    -> {AST, UcdFuns}
            end;
        _ ->
            {AST, UcdFuns}
    end.

collect_ucd_funs_2(NameAST, Arity, UcdFuns) ->
    case erl_syntax:atom_name(NameAST) of
        "ucd_" ++ _ ->
            Fun = {erl_syntax:atom_value(NameAST), Arity},
            sets:add_element(Fun, UcdFuns);
        _ ->
            UcdFuns
    end.


collect_ucd_funs_replace({Name, _}=Fun, CP, UcdFuns) ->
    NewAST = ?Q("'@Name@'(_@CP)"),
    {NewAST, sets:add_element(Fun, UcdFuns)}.


collect_ucd_category_fun(AST, CP, V, UcdFuns, Name, ValuesFun) ->
    case erl_syntax:type(V) of
        atom ->
            collect_ucd_category_fun_1(AST, CP, [erl_syntax:atom_value(V)],
                                       UcdFuns, Name, ValuesFun);
        list ->
            case lists:all(fun (E) -> erl_syntax:type(E) == atom end,
                           erl_syntax:list_elements(V)) of
                true ->
                    collect_ucd_category_fun_1(AST, CP, erl_syntax:concrete(V),
                                               UcdFuns, Name, ValuesFun);
                false ->
                    {AST, UcdFuns}
            end;
        _ ->
            {AST, UcdFuns}
    end.

collect_ucd_category_fun_1(AST, CP, Values, UcdFuns, Name, ValuesFun) ->
    case lists:all(fun (C) -> lists:member(C, ValuesFun()) end , Values) of
        true ->
            NewName = ucd_fun_name(Name, Values),
            NewAST = ?Q("'@NewName@'(_@CP)"),
            {NewAST, sets:add_element({Name, Values}, UcdFuns)};
        false ->
            {AST, UcdFuns}
    end.


collect_ucd_property_fun(AST, CP, V, UcdFuns, Name, ValuesFun) ->
    case erl_syntax:type(V) of
        atom ->
            collect_ucd_property_fun_1(AST, CP, erl_syntax:atom_value(V),
                                       UcdFuns, Name, ValuesFun);
        _ ->
            {AST, UcdFuns}
    end.

collect_ucd_property_fun_1(AST, CP, Property, UcdFuns, Name, ValuesFun) ->
    case lists:member(Property, ValuesFun()) of
        true ->
            NewName = ucd_fun_name(Name, [Property]),
            NewAST = ?Q("'@NewName@'(_@CP)"),
            {NewAST, sets:add_element({Name, Property}, UcdFuns)};
        false ->
            {AST, UcdFuns}
    end.


forms(Funs) ->
    State = #{ data => undefined
             , common_properties => undefined
             , ranges => undefined
             , blocks => undefined
             , properties_list => undefined
             , special_casing => undefined
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

forms({ucd_numeric, 1}, State0) ->
    {Data, State1} = ucd_data(State0),
    {ucd_codegen:numeric_funs_ast(Data, ucd_unihan:numeric_data()), State1};

forms({ucd_blocks, 0}, State0) ->
    {Blocks, State1} = blocks_data(State0),
    Data = [{Block, Range} || {Range, Block} <- Blocks],
    {[?Q("ucd_blocks() -> _@Data@.")], State1};

forms({ucd_block, 1}, State0) ->
    {Blocks, State1} = blocks_data(State0),
    {[ucd_codegen:block_fun_ast(Blocks)], State1};

forms({ucd_decomposition, 1}, State0) ->
    {Data, State1} = ucd_data(State0),
    {ucd_codegen:decomposition_funs_ast(Data), State1};

forms({ucd_grapheme_break, 1}, State) ->
    {[ucd_codegen:grapheme_break_fun_ast()], State};

forms({ucd_grapheme_break, Classes}, State) ->
    Name = ucd_fun_name(ucd_grapheme_break, Classes),
    {[ucd_codegen:grapheme_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_word_break, 1}, State) ->
    {[ucd_codegen:word_break_fun_ast()], State};

forms({ucd_word_break, Classes}, State) ->
    Name = ucd_fun_name(ucd_word_break, Classes),
    {[ucd_codegen:word_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_sentence_break, 1}, State) ->
    {[ucd_codegen:sentence_break_fun_ast()], State};

forms({ucd_sentence_break, Classes}, State) ->
    Name = ucd_fun_name(ucd_sentence_break, Classes),
    {[ucd_codegen:sentence_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_line_break, 1}, State) ->
    {[ucd_codegen:line_break_fun_ast()], State};

forms({ucd_line_break, Classes}, State) ->
    Name = ucd_fun_name(ucd_line_break, Classes),
    {[ucd_codegen:line_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_is_category, Categories}, State) ->
    Name = ucd_fun_name(ucd_is_category, Categories),
    is_category_forms(Name, Categories, State);

forms({ucd_has_property, Property}, State) ->
    Name = ucd_fun_name(ucd_has_property, [Property]),
    has_property_forms(Name, Property, State);

forms({ucd_lowercase_mapping, 1}, State0) ->
    {Data, State1} = ucd_data(State0),
    {ucd_codegen:lowercase_mapping_funs_ast(Data), State1};

forms({ucd_uppercase_mapping, 1}, State0) ->
    {Data, State1} = ucd_data(State0),
    {ucd_codegen:uppercase_mapping_funs_ast(Data), State1};

forms({ucd_titlecase_mapping, 1}, State0) ->
    {Data, State1} = ucd_data(State0),
    {ucd_codegen:titlecase_mapping_funs_ast(Data), State1};

forms({ucd_special_casing_upper, 1}, State) ->
    ucd_special_casing_forms(upper, State);

forms({ucd_special_casing_lower, 1}, State) ->
    ucd_special_casing_forms(lower, State);

forms({ucd_special_casing_title, 1}, State) ->
    ucd_special_casing_forms(title, State);

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


ucd_special_casing_forms(Mapping, State0) ->
    {Data, State1} = special_casing_data(State0),
    {ucd_codegen:special_casing_funs_ast(Data, Mapping), State1}.


is_category_forms(Name, Categories, State0) ->
    {Data, State1} = ucd_data(State0),
    Forms = [ucd_codegen:is_category_fun_ast(Name, Data, Categories)],
    {Forms, State1}.


has_property_forms(Name, Property, State0) ->
    {Data, State1} = ucd_proplist(State0),
    Forms = [ucd_codegen:has_property_fun_ast(Name, Data, Property)],
    {Forms, State1}.


is_function_generated(Name, #{generated_functions := Funs}) ->
    sets:is_element(Name, Funs).


function_generated(Name, #{generated_functions := Funs}=State) ->
    State#{generated_functions := sets:add_element(Name, Funs)}.


ucd_data(#{data := undefined}=State) ->
    Data = ucd_properties:unicode_data(),
    {Data, State#{data := Data}};

ucd_data(#{data := Data}=State) ->
    {Data, State}.


common_properties_data(#{common_properties := undefined}=State0) ->
    {Data, State1} = ucd_data(State0),
    Properties = ucd_properties:common_properties(Data),
    {Properties, State1#{common_properties := Properties}};

common_properties_data(#{common_properties := Properties}=State) ->
    {Properties, State}.


ranges_data(#{ranges := undefined}=State0) ->
    {Data, State1} = ucd_data(State0),
    Ranges = ucd_properties:ranges(Data),
    {Ranges, State1#{ranges := Ranges}};

ranges_data(#{ranges := Ranges}=State) ->
    {Ranges, State}.


blocks_data(#{blocks := undefined}=State) ->
    Blocks = ucd_names:blocks(),
    {Blocks, State#{blocks := Blocks}};

blocks_data(#{blocks := Blocks}=State) ->
    {Blocks, State}.


ucd_proplist(#{properties_list := undefined}=State) ->
    PropList = ucd_properties:properties_list(),
    {PropList, State#{properties_list := PropList}};

ucd_proplist(#{properties_list := PropList}=State) ->
    {PropList, State}.


special_casing_data(#{special_casing := undefined}=State0) ->
    Data = ucd_casing:special_casing(),
    {Data, State0#{special_casing := Data}};

special_casing_data(#{special_casing := Data}=State) ->
    {Data, State}.


ucd_fun_name(Prefix, Parts) ->
    Suffix = string:join([atom_to_list(P) || P <- Parts], "_"),
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ Suffix).
