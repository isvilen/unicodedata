-module(unicodedata_ucd_transform).
-export([parse_transform/2, format_error/1]).

-include_lib("syntax_tools/include/merl.hrl").

parse_transform(Forms, _Opts) ->
    {Forms1, UcdFuns} = collect_ucd_funs(Forms),
    Forms1 ++ forms(UcdFuns).


format_error({Fmt, Args}) ->
    lists:flatten(io_lib:format(Fmt, Args)).


collect_ucd_funs(Forms) ->
    File = get_file(Forms),
    Fun = fun (Form, UcdFuns) -> collect_ucd_funs(File, Form, UcdFuns) end,
    {Forms1, UcdFuns} = lists:mapfoldr(Fun, sets:new(), Forms),
    {erl_syntax:revert_forms(Forms1), sets:to_list(UcdFuns)}.

collect_ucd_funs(File, Form, UcdFuns0) ->
    Fun = fun (AST, UcdFuns1) -> collect_ucd_funs_1(File, AST, UcdFuns1) end,
    erl_syntax_lib:mapfold(Fun, UcdFuns0, Form).

collect_ucd_funs_1(File, AST, UcdFuns) ->
    case AST of
        ?Q("ucd_is_category(_@CP, _@V)") ->
            collect_ucd_fun(ucd_is_category, [CP, V], File, UcdFuns);

        ?Q("ucd_has_property(_@CP, _@V)") ->
            collect_ucd_fun(ucd_has_property, [CP, V], File, UcdFuns);

        ?Q("ucd_name_aliases(_@CP, _@V)") ->
            collect_ucd_fun(ucd_name_aliases, [CP, V], File, UcdFuns);

        ?Q("ucd_name_alias_lookup(_@Name, _@V)") ->
            collect_ucd_fun(ucd_name_alias_lookup, [Name, V], File, UcdFuns);

        ?Q("ucd_grapheme_break(_@CP, _@V)") ->
            collect_ucd_fun(ucd_grapheme_break, [CP, V], File, UcdFuns);

        ?Q("ucd_word_break(_@CP, _@V)") ->
            collect_ucd_fun(ucd_word_break, [CP, V], File, UcdFuns);

        ?Q("ucd_sentence_break(_@CP, _@V)") ->
            collect_ucd_fun(ucd_sentence_break, [CP, V], File, UcdFuns);

        ?Q("ucd_line_break(_@CP, _@V)") ->
            collect_ucd_fun(ucd_line_break, [CP, V], File, UcdFuns);

        ?Q("ucd_special_casing(_@CP, _@V)") ->
            collect_ucd_fun_replace(ucd_special_casing, [CP, V], File, UcdFuns);

        ?Q("ucd_combining_class(_@CP, _@V)") ->
            collect_ucd_fun(ucd_combining_class, [CP, V], File, UcdFuns);

        ?Q("'@Name'(_@@Args)") ->
            collect_ucd_funs_2(AST, Name, length(Args), UcdFuns);

        ?Q("fun '@Name'/90919") ->
            collect_ucd_funs_2(AST, Name, erl_syntax:concrete(Q1), UcdFuns);

        _ ->
            {AST, UcdFuns}
    end.

collect_ucd_funs_2(AST, NameAST, Arity, UcdFuns0) ->
    UcdFuns1 =
        case erl_syntax:type(NameAST) of
            atom ->
                case erl_syntax:atom_name(NameAST) of
                    "ucd_" ++ _ ->
                        FunName = erl_syntax:atom_value(NameAST),
                        sets:add_element({FunName, Arity}, UcdFuns0);
                    _ ->
                        UcdFuns0
                end;
            _ ->
                UcdFuns0
        end,
    {AST, UcdFuns1}.


collect_ucd_fun(FunName, FunArgs, File, UcdFuns) ->
    Args = ucd_fun_args(FunName, FunArgs, File),
    NewName = ucd_fun_unique_name(FunName, sets:size(UcdFuns)),
    NewAST = ucd_fun_new_ast(FunName, NewName, FunArgs),
    {NewAST, sets:add_element({FunName, NewName, Args}, UcdFuns)}.


collect_ucd_fun_replace(FunName, FunArgs, File, UcdFuns) ->
    Args = ucd_fun_args(FunName, FunArgs, File),
    NewName = ucd_fun_replace_name(FunName, Args),
    NewAST = ucd_fun_new_ast(FunName, NewName, FunArgs),
    {NewAST, sets:add_element({FunName, Args}, UcdFuns)}.


ucd_fun_unique_name(Prefix, UniqueId) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ integer_to_list(UniqueId)).


ucd_fun_replace_name(ucd_special_casing, lower) -> ucd_special_casing_lower;
ucd_fun_replace_name(ucd_special_casing, title) -> ucd_special_casing_title;
ucd_fun_replace_name(ucd_special_casing, upper) -> ucd_special_casing_upper.


ucd_fun_new_ast(_, Name, [Arg, _]) ->
    ?Q("'@Name@'(_@Arg)").


ucd_fun_args(ucd_is_category, [_, V], File) ->
    Values = ucd_fun_atom_list_arg(ucd_is_category, 2, V, File),
    ucd_fun_validate_arg(Values, unicodedata_ucd:categories(), File, V);

ucd_fun_args(ucd_has_property, [_, V], File) ->
    Value = ucd_fun_atom_arg(ucd_has_property, 2, V, File),
    ucd_fun_validate_arg(Value, unicodedata_ucd:prop_list_types(), File, V);

ucd_fun_args(ucd_name_aliases, [_, V], File) ->
    Value = ucd_fun_atom_arg(ucd_name_aliases, 2, V, File),
    ucd_fun_validate_arg(Value, unicodedata_ucd:name_aliases_types(), File, V);

ucd_fun_args(ucd_name_alias_lookup, [_, V], File) ->
    Value = ucd_fun_atom_list_arg(ucd_name_alias_lookup, 2, V, File),
    ucd_fun_validate_arg(Value, unicodedata_ucd:name_aliases_types(), File, V);

ucd_fun_args(ucd_grapheme_break, [_, V], File) ->
    Value = ucd_fun_atom_list_arg(ucd_grapheme_break, 2, V, File),
    ucd_fun_validate_arg(Value, unicodedata_ucd:grapheme_break_classes(), File, V);

ucd_fun_args(ucd_word_break, [_, V], File) ->
    Value = ucd_fun_atom_list_arg(ucd_word_break, 2, V, File),
    ucd_fun_validate_arg(Value, unicodedata_ucd:word_break_classes(), File, V);

ucd_fun_args(ucd_sentence_break, [_, V], File) ->
    Value = ucd_fun_atom_list_arg(ucd_sentence_break, 2, V, File),
    ucd_fun_validate_arg(Value, unicodedata_ucd:sentence_break_classes(), File, V);

ucd_fun_args(ucd_line_break, [_, V], File) ->
    Value = ucd_fun_atom_list_arg(ucd_line_break, 2, V, File),
    ucd_fun_validate_arg(Value, unicodedata_ucd:line_break_classes(), File, V);

ucd_fun_args(ucd_special_casing, [_, V], File) ->
    Value = ucd_fun_atom_arg(ucd_special_casing, 2, V, File),
    ucd_fun_validate_arg(Value, [upper, lower, title], File, V);

ucd_fun_args(ucd_combining_class, [_, Arg], File) ->
    case Arg of
        ?Q("not _@Arg1") ->
            {'not', ucd_fun_int_list_arg(ucd_combining_class, 2, Arg1, File)};
        _  ->
            ucd_fun_int_list_arg(ucd_combining_class, 2, Arg, File)
    end.


ucd_fun_atom_arg(FunName, ArgPos, Arg, File) ->
    case erl_syntax:type(Arg) of
        atom -> erl_syntax:atom_value(Arg);
        _    -> error(File, Arg, "atom expected as argument ~p of ~s function",
                      [ArgPos, FunName])
    end.


ucd_fun_atom_list_arg(FunName, ArgPos, Arg, File) ->
    try erl_syntax:concrete(Arg) of
        V  when is_atom(V) ->
            [V];
        Vs when is_list(Vs) ->
            case lists:all(fun erlang:is_atom/1, Vs) of
                true ->
                    Vs;
                false ->
                    error(File, Arg,
                          "list of atoms expected as argument ~p of ~s function",
                          [ArgPos, FunName])
            end;
        _ ->
            error(File, Arg,
                  "atom or list of atoms expected as argument ~p of ~s function",
                  [ArgPos, FunName])
    catch
        error:badarg ->
            error(File, Arg, "invalid argument ~p of ~s function",
                  [ArgPos, FunName])
    end.


ucd_fun_int_list_arg(FunName, ArgPos, Arg, File) ->
    try erl_syntax:concrete(Arg) of
        V  when is_integer(V) ->
            [V];
        Vs when is_list(Vs) ->
            case lists:all(fun erlang:is_integer/1, Vs) of
                true ->
                    Vs;
                false ->
                    error(File, Arg,
                          "list of integers expected as argument ~p of ~s function",
                          [ArgPos, FunName])
            end;
        _ ->
            error(File, Arg,
                  "integer or list of integers expected as argument ~p of ~s function",
                  [ArgPos, FunName])
    catch
        error:badarg ->
            error(File, Arg, "invalid argument ~p of ~s function",
                  [ArgPos, FunName])
    end.


ucd_fun_validate_arg(Values, AllowedValues, File, AST) when is_list(Values) ->
    ucd_fun_validate_arg_values(Values, AllowedValues, File, AST), Values;

ucd_fun_validate_arg(Value, AllowedValues, File, AST) ->
    ucd_fun_validate_arg_value(Value, AllowedValues, File, AST), Value.


ucd_fun_validate_arg_values([], _, _, _) ->
    ok;

ucd_fun_validate_arg_values([V|Vs], AllowedValues, File, AST) ->
    ucd_fun_validate_arg_value(V, AllowedValues, File, AST),
    ucd_fun_validate_arg_values(Vs, AllowedValues, File, AST).


ucd_fun_validate_arg_value(Value, AllowedValues, File, AST) ->
    case lists:member(Value, AllowedValues) of
        true  -> ok;
        false -> error(File, AST, "invalid value ~p", [Value])
    end.


forms(Funs) ->
    State = #{ data => undefined
             , common_properties => undefined
             , ranges => undefined
             , bidi_mirroring => undefined
             , blocks => undefined
             , prop_list => undefined
             , special_casing => undefined
             , normalization_properties => undefined
             , name_aliases => undefined
             , generated_functions => sets:new()
             },
    {Forms, _} = lists:mapfoldl(fun forms/2, State, Funs),
    erl_syntax:revert_forms(lists:append(Forms)).


forms({ucd_category, 1}, State0) ->
    {Properties, State1} = common_properties_data(State0),
    {Ranges, State2} = ranges_data(State1),
    Forms = [ category_data_fun_ast(Properties)
            , category_range_data_fun_ast(Ranges)
            , category_fun_ast()
            ],
    ensure_common_properties_index(Forms, State2);

forms({ucd_combining_class, 1}, State0) ->
    {Properties, State1} = common_properties_data(State0),
    {Ranges, State2} = ranges_data(State1),
    Forms = [ combining_class_data_fun_ast(Properties)
            , combining_class_range_data_fun_ast(Ranges)
            , combining_class_fun_ast()
            ],
    ensure_common_properties_index(Forms, State2);

forms({ucd_combining_class, Name, Classes}, State0) ->
    {Data, State1} = ucd_data(State0),
    {[combining_class_fun_ast(Name, Classes, Data)], State1};

forms({ucd_bidi_class, 1}, State0) ->
    {Properties, State1} = common_properties_data(State0),
    {Ranges, State2} = ranges_data(State1),
    Forms = [ bidi_class_data_fun_ast(Properties)
            , bidi_class_range_data_fun_ast(Ranges)
            , bidi_class_fun_ast()
            ],
    ensure_common_properties_index(Forms, State2);

forms({ucd_bidi_mirrored, 1}, State0) ->
    {Properties, State1} = common_properties_data(State0),
    {Ranges, State2} = ranges_data(State1),
    Forms = [ bidi_mirrored_data_fun_ast(Properties)
            , bidi_mirrored_range_data_fun_ast(Ranges)
            , bidi_mirrored_fun_ast()
            ],
    ensure_common_properties_index(Forms, State2);

forms({ucd_bidi_mirroring_glyph, 1}, State0) ->
    {Data, State1} = bidi_mirroring_data(State0),
    {[bidi_mirroring_glyph_fun_ast(Data)], State1};

forms({ucd_bidi_brackets, 1}, State0) ->
    {CPs, State1} = codepoints_data(State0),
    {BidiMirroring, State2} = bidi_mirroring_data(State1),
    {DecompData, State3} = decomposition_data(State2),
    {[bidi_brackets_fun_ast(BidiMirroring, CPs, DecompData)], State3};

forms({ucd_numeric, 1}, State0) ->
    {Data, State1} = numeric_data(State0),
    {[ numeric_index_fun_ast(Data)
     , numeric_data_fun_ast(Data)
     , numeric_fun_ast()
     ], State1};

forms({ucd_blocks, 0}, State0) ->
    {Blocks, State1} = blocks_data(State0),
    Data = [{Block, Range} || {Range, Block} <- Blocks],
    {[?Q("ucd_blocks() -> _@Data@.")], State1};

forms({ucd_block, 1}, State0) ->
    {Blocks, State1} = blocks_data(State0),
    {[block_fun_ast(Blocks)], State1};

forms({ucd_decomposition, 1}, State0) ->
    {Data, State1} = decomposition_data(State0),
    {[ decomposition_index_fun_ast(Data)
     , decomposition_data_fun_ast(Data)
     , decomposition_fun_ast()
     ]
    , State1};

forms({ucd_composition, 2}, State0) ->
    {Data, State1} = composition_data(State0),
    {[?Q(["ucd_composition(CP1, CP2) ->"
         ,"    maps:get({CP1, CP2}, _@Data@, undefined)."
         ])]
    , State1};

forms({ucd_hangul_syllable_type, 1}, State) ->
    {[hangul_syllable_type_fun_ast()], State};

forms({ucd_grapheme_break, 1}, State) ->
    {[grapheme_break_fun_ast()], State};

forms({ucd_grapheme_break, Name, Classes}, State) ->
    {[grapheme_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_word_break, 1}, State) ->
    {[word_break_fun_ast()], State};

forms({ucd_word_break, Name, Classes}, State) ->
    {[word_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_sentence_break, 1}, State) ->
    {[sentence_break_fun_ast()], State};

forms({ucd_sentence_break, Name, Classes}, State) ->
    {[sentence_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_line_break, 1}, State) ->
    {[line_break_fun_ast()], State};

forms({ucd_line_break, Name, Classes}, State) ->
    {[line_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_is_category, Name, Categories}, State0) ->
    {Data, State1} = ucd_data(State0),
    {[is_category_fun_ast(Name, Data, Categories)], State1};

forms({ucd_has_property, Name, Property}, State0) ->
    {Data, State1} = prop_list_data(State0),
    {[has_property_fun_ast(Name, Data, Property)], State1};

forms({ucd_name_aliases, Name, Type}, State0) ->
    {Data, State1} = name_aliases_data(State0),
    {[name_aliases_fun_ast(Name, Type, Data)], State1};

forms({ucd_lowercase_mapping, 1}, State0) ->
    {Data, State1} = lowercase_mapping_data(State0),
    {case_mapping_funs_ast(Data
                          ,ucd_lowercase_mapping
                          ,ucd_lowercase_mapping_idx
                          ,ucd_lowercase_mapping_data)
    , State1};

forms({ucd_uppercase_mapping, 1}, State0) ->
    {Data, State1} = uppercase_mapping_data(State0),
    {case_mapping_funs_ast(Data
                          ,ucd_uppercase_mapping
                          ,ucd_uppercase_mapping_idx
                          ,ucd_uppercase_mapping_data)
    , State1};

forms({ucd_titlecase_mapping, 1}, State0) ->
    {Data, State1} = titlecase_mapping_data(State0),
    {case_mapping_funs_ast(Data
                          ,ucd_titlecase_mapping
                          ,ucd_titlecase_mapping_idx
                          ,ucd_titlecase_mapping_data)
    , State1};

forms({ucd_special_casing, Mapping}, State0) ->
    {Data, State1} = special_casing_data(State0),
    {special_casing_funs_ast(Data, Mapping), State1};

forms({ucd_nfc_quick_check, 1}, State0) ->
    {Data, State1} = normalization_properties(State0),
    {[nfc_quick_check_fun_ast(Data)], State1};

forms({ucd_nfkc_quick_check, 1}, State0) ->
    {Data, State1} = normalization_properties(State0),
    {[nfkc_quick_check_fun_ast(Data)], State1};

forms({ucd_nfd_quick_check, 1}, State0) ->
    {Data, State1} = normalization_properties(State0),
    {[nfd_quick_check_fun_ast(Data)], State1};

forms({ucd_nfkd_quick_check, 1}, State0) ->
    {Data, State1} = normalization_properties(State0),
    {[nfkd_quick_check_fun_ast(Data)], State1};

forms({ucd_codepoint_range, 1}, State0) ->
    {Ranges, State1} = ranges_data(State0),
    {[codepoint_range_fun_ast(Ranges)], State1};

forms({ucd_east_asian_width, 1}, State) ->
    {[ east_asian_width_data_fun_ast()
     , east_asian_width_defaults_fun_ast()
     , east_asian_width_fun_ast()
     ], State};

forms({ucd_codepoint_name, 1}, State0) ->
    {Data, State1} = codepoints_data(State0),
    ensure_common_properties_index(codepoint_name_funs_ast(Data), State1);

forms({ucd_codepoint_name_lookup, 1}, State0) ->
    {CPs, State1} = codepoints_data(State0),
    Data = name_lookup_codepoint_names(CPs),
    {Forms, State2} = name_lookup_funs_ast(ucd_codepoint_name_lookup, Data, State1),
    ensure_name_lookup_helper_funs(Forms, State2);

forms({ucd_name_alias_lookup, Name, Types}, State0) ->
    {Data, State1} = name_aliases_data(State0),
    Data1 = name_lookup_alias_names(Data, Types),
    {Forms, State2} = name_lookup_funs_ast(Name, Data1, State1),
    ensure_name_lookup_helper_funs(Forms, State2);

forms({ucd_normalize_name, 1}, State) ->
    ensure_normalize_name_fun([], State);

forms(_, State) ->
    {[], State}.


ensure_common_properties_index(Forms, State0) ->
    {Properties, State1} = common_properties_data(State0),
    ensure_fun(ucd_properties_idx
              , fun () -> [common_properties_index_fun_ast(Properties)] end
              , Forms, State1).


ensure_name_lookup_helper_funs(Forms, State0) ->
    {Forms1, State1} = ensure_normalize_name_fun(Forms, State0),
    ensure_name_lookup_search_fun(Forms1, State1).


ensure_normalize_name_fun(Forms, State) ->
    ensure_fun(ucd_normalize_name, fun normalize_name_funs_ast/0, Forms, State).


ensure_name_lookup_search_fun(Forms, State) ->
    ensure_fun(ucd_name_lookup, fun () -> [name_lookup_search_fun()] end
              ,Forms, State).


ensure_name_lookup_collision_fun_ast(Forms, State) ->
    ensure_fun(ucd_name_lookup_collision
              , fun () -> [name_lookup_collision_fun_ast()] end
              , Forms, State).


ensure_fun(Name, FormsFun, Forms0, #{generated_functions := Funs}=State0) ->
    case sets:is_element(Name, Funs) of
        true  ->
            {Forms0, State0};
        false ->
            Forms1 = FormsFun() ++  Forms0,
            State1 = State0#{generated_functions := sets:add_element(Name, Funs)},
            {Forms1, State1}
    end.


ucd_data(#{data := undefined}=State) ->
    Data = unicodedata_ucd:unicode_data(),
    {Data, State#{data := Data}};

ucd_data(#{data := Data}=State) ->
    {Data, State}.


codepoints_data(State) ->
    {Data, State1} = ucd_data(State),
    {unicodedata_ucd:codepoints(Data), State1}.


common_properties_data(#{common_properties := undefined}=State0) ->
    {Data, State1} = codepoints_data(State0),
    Properties = [{Id, Name, Cat, Comb, Bidi, Mirrored}
                  || {Id,Name,Cat,Comb,Bidi,_,_,Mirrored,_,_,_} <- Data],
    {Properties, State1#{common_properties := Properties}};

common_properties_data(#{common_properties := Properties}=State) ->
    {Properties, State}.


ranges_data(#{ranges := undefined}=State0) ->
    {Data, State1} = ucd_data(State0),
    Ranges = unicodedata_ucd:ranges(Data),
    {Ranges, State1#{ranges := Ranges}};

ranges_data(#{ranges := Ranges}=State) ->
    {Ranges, State}.


bidi_mirroring_data(#{bidi_mirroring := undefined}=State) ->
    Data = unicodedata_ucd:bidi_mirroring(),
    {Data, State#{bidi_mirroring := Data}};

bidi_mirroring_data(#{bidi_mirroring := Data}=State) ->
    {Data, State}.


blocks_data(#{blocks := undefined}=State) ->
    Blocks = unicodedata_ucd:blocks(),
    {Blocks, State#{blocks := Blocks}};

blocks_data(#{blocks := Blocks}=State) ->
    {Blocks, State}.


prop_list_data(#{prop_list := undefined}=State) ->
    PropList = unicodedata_ucd:prop_list(),
    {PropList, State#{prop_list := PropList}};

prop_list_data(#{prop_list := PropList}=State) ->
    {PropList, State}.


special_casing_data(#{special_casing := undefined}=State0) ->
    Data = unicodedata_ucd:special_casing(),
    {Data, State0#{special_casing := Data}};

special_casing_data(#{special_casing := Data}=State) ->
    {Data, State}.


normalization_properties(#{normalization_properties := undefined}=State0) ->
    Data = unicodedata_ucd:derived_normalization_props(),
    {Data, State0#{normalization_properties := Data}};

normalization_properties(#{normalization_properties := Data}=State) ->
    {Data, State}.


numeric_data(State0) ->
    {Data, State1} = codepoints_data(State0),
    Result = [{Id, Numeric} || {Id,_,_,_,_,_,Numeric,_,_,_,_} <- Data
                             , Numeric /= undefined],
    {Result, State1}.


name_aliases_data(#{name_aliases := undefined}=State0) ->
    Data = unicodedata_ucd:name_aliases(),
    {Data, State0#{name_aliases := Data}};

name_aliases_data(#{name_aliases := Data}=State) ->
    {Data, State}.


uppercase_mapping_data(State0) ->
    {Data, State1} = codepoints_data(State0),
    Result = [{Id, Upper} || {Id,_,_,_,_,_,_,_,Upper,_,_} <- Data
                          , Upper /= undefined],
    {Result, State1}.


lowercase_mapping_data(State0) ->
    {Data, State1} = codepoints_data(State0),
    Result = [{Id, Lower} || {Id,_,_,_,_,_,_,_,_,Lower,_} <- Data
                           , Lower /= undefined],
    {Result, State1}.


titlecase_mapping_data(State0) ->
    {Data, State1} = codepoints_data(State0),
    Result = [{Id, Title} || {Id,_,_,_,_,_,_,_,_,_,Title} <- Data
                           , Title /= undefined],
    {Result, State1}.


decomposition_data(State0) ->
    {Data, State1} = codepoints_data(State0),
    Result = [{Id, Decomp} || {Id,_,_,_,_,Decomp,_,_,_,_,_} <- Data
                            , Decomp /= undefined],
    {Result, State1}.


composition_data(State0) ->
    {Data, State1} = ucd_data(State0),
    NonStarters = lists:foldl(fun composition_non_starter/2, sets:new(), Data),
    Exclusions = sets:from_list(unicodedata_ucd:composition_exclusions()),
    {lists:foldr(fun (CP, Acc) ->
                      composition_data(CP, NonStarters, Exclusions, Acc)
                 end, #{}, Data)
    , State1}.


composition_non_starter({_,_,_,0,_,_,_,_,_,_,_}, Set) ->
    Set;
composition_non_starter({CP,_,_,_,_,_,_,_,_,_,_}, Set) when is_integer(CP) ->
    sets:add_element(CP, Set).


composition_data({CP,_,_,0,_,[CP1,CP2],_,_,_,_,_}, NonStarters, Exclusions, Acc) ->
    case sets:is_element(CP1, NonStarters) of
        true  -> Acc;
        false -> case sets:is_element(CP, Exclusions) of
                     true  -> Acc;
                     false -> maps:put({CP1,CP2}, CP, Acc)
                 end
    end;

composition_data(_, _, _, Acc) ->
    Acc.


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
    index_fun_ast(ucd_properties_idx, compact(CommonProperties)).


category_data_fun_ast(CommonProperties) ->
    Categories = unicodedata_ucd:categories(),
    Names = [unicodedata_ucd:category_name(C) || C <- Categories],
    DecodeASTFun = fun (V) -> decode_value_case_ast(V, Names) end,
    Data = [element(3,V) || V <- CommonProperties],
    Bits = encode_to_bits(Categories, Data),
    data_fun_ast(ucd_category_data, Bits, DecodeASTFun).


category_range_data_fun_ast(Ranges) ->
    RangeValues = [{F,T,unicodedata_ucd:category_name(C)}
                   || {{F,T}, _,C,_,_,_,_,_,_,_,_} <- Ranges],
    Default = unicodedata_ucd:category_name('Cn'),
    range_fun_ast(ucd_category_range_data, RangeValues, ?Q("_@Default@")).


category_fun_ast() ->
    ?Q(["ucd_category(CP) ->"
       ,"  case ucd_properties_idx(CP) of"
       ,"    undefined -> ucd_category_range_data(CP);"
       ,"    Idx       -> ucd_category_data(Idx)"
       ,"  end."]).


is_category_fun_ast(Name, Data, Categories) ->
    Data1 = [element(1,V) || V <- Data, lists:member(element(3,V), Categories)],
    Ranges = [{F, T, true} || {F, T} <- compact_ranges(Data1)],
    range_fun_ast(Name, Ranges, ?Q("false")).


has_property_fun_ast(Name, Properties, Property) ->
    Data1 = [V || {V, Prop} <- Properties, Prop == Property],
    Ranges = [{F, T, true} || {F, T} <- compact_ranges(Data1)],
    range_fun_ast(Name, Ranges, ?Q("false")).


combining_class_data_fun_ast(CommonProperties) ->
    Data = [element(4,V) || V <- CommonProperties],
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


combining_class_fun_ast(Name, Classes, Data) ->
    Cs = case Classes of
             {'not', Vs} ->
                 AllVs = sets:from_list(lists:seq(0, 240)),
                 sets:subtract(AllVs, sets:from_list(Vs));
             _ ->
                 sets:from_list(Classes)
         end,
    Data1 = [CP || {CP, _,_,CC,_,_,_,_,_,_,_} <- Data, sets:is_element(CC, Cs)],
    Ranges = [{F, T, true} || {F, T} <- compact_ranges(Data1)],
    range_fun_ast(Name, Ranges, ?Q("false")).


bidi_class_data_fun_ast(CommonProperties) ->
    Classes = unicodedata_ucd:bidi_classes(),
    Names = [unicodedata_ucd:bidi_class_name(C) || C <- Classes],
    DecodeASTFun = fun (V) -> decode_value_case_ast(V, Names) end,
    Data = [element(5,V) || V <- CommonProperties],
    Bits = encode_to_bits(Classes, Data),
    data_fun_ast(ucd_bidi_class_data, Bits, DecodeASTFun).


bidi_class_range_data_fun_ast(Ranges) ->
    RangeValues = [{F,T,unicodedata_ucd:bidi_class_name(C)}
                   || {{F,T}, _,_,_,C,_,_,_,_,_,_} <- Ranges],
    range_fun_ast(ucd_bidi_class_range_data, RangeValues).


bidi_class_fun_ast() ->
    RangeValues = [{F,T,unicodedata_ucd:bidi_class_name(C)}
                   || {F,T,C} <- unicodedata_ucd:bidi_class_defaults()],
    Default = unicodedata_ucd:bidi_class_name('L'),
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


bidi_mirrored_data_fun_ast(CommonProperties) ->
    Values = [false, true],
    DecodeASTFun = fun (V) -> decode_value_case_ast(V, Values) end,
    Data = [element(6,V) || V <- CommonProperties],
    Bits = encode_to_bits(Values, Data),
    data_fun_ast(ucd_bidi_mirrored_data, Bits, DecodeASTFun).


bidi_mirrored_range_data_fun_ast(Ranges) ->
    RangeValues = [{F,T,V}
                   || {{F,T}, _,_,_,_,_,_,V,_,_,_} <- Ranges],
    range_fun_ast(ucd_bidi_mirrored_range_data, RangeValues, ?Q("false")).


bidi_mirrored_fun_ast() ->
    ?Q(["ucd_bidi_mirrored(CP) ->"
       ,"  case ucd_properties_idx(CP) of"
       ,"    undefined -> ucd_bidi_mirrored_range_data(CP);"
       ,"    Idx       -> ucd_bidi_mirrored_data(Idx)"
       ,"  end."]).


bidi_mirroring_glyph_fun_ast(Data) ->
    Rs = range_values(unicodedata_ucd:sort_by_codepoints(Data)),
    range_fun_ast(ucd_bidi_mirroring_glyph, Rs, ?Q("none")).


bidi_brackets_fun_ast(BidiMirroring, CPs, Decomp) ->
    Data = bidi_brackets_data(BidiMirroring, CPs, Decomp),
    Rs = range_values([{CP, {Type, OCPs}} || {Type, CP, OCPs} <- Data]),
    range_fun_ast(ucd_bidi_brackets, Rs, ?Q("none")).


bidi_brackets_data(BidiMirroring, Codepoints, Decomp) ->
    BidiMap = maps:from_list(BidiMirroring),
    BracketsMap = brackets_data(Codepoints),
    EqMap = canonical_equivalence_data(Decomp),
    bidi_brackets_data(BidiMirroring, BidiMap, BracketsMap, EqMap, []).


bidi_brackets_data([], _, _, _, Acc) ->
    lists:reverse(Acc);

bidi_brackets_data([{CP1, CP2} | Rest], BidiMap, BracketsMap, EqMap, AccIn) ->
    AccOut = case maps:get(CP1, BracketsMap, undefined) of
                 undefined ->
                     AccIn;
                 Type ->
                     OCPs = bidi_bracket_opposites(CP1, CP2, BidiMap, EqMap),
                     [{Type, CP1, OCPs} | AccIn]
             end,
    bidi_brackets_data(Rest, BidiMap, BracketsMap, EqMap, AccOut).


bidi_bracket_opposites(CP1, CP2, BidiMap, EqMap) ->
    case maps:get(CP1, EqMap, undefined) of
        undefined ->
            [CP2];
        CP ->
            case maps:get(CP, BidiMap, undefined) of
                undefined -> [CP2];
                DCP2      -> [CP2, DCP2]
            end
    end.


brackets_data(CPs) -> brackets_data(CPs, #{}).

brackets_data([], Acc) ->
    Acc;

brackets_data([{CP,_,Cat,_,'ON',_,_,true,_,_,_} | CPs], Acc0) ->
    Acc1 = case Cat of
        'Ps' -> Acc0#{CP => open};
        'Pe' -> Acc0#{CP => close};
        _    -> Acc0
    end,
    brackets_data(CPs, Acc1);

brackets_data([_ | CPs], Acc) ->
    brackets_data(CPs, Acc).


canonical_equivalence_data(Decomp) ->
    canonical_equivalence_data(Decomp, #{}).

canonical_equivalence_data([], Acc) ->
    Acc;

canonical_equivalence_data([{CP1, [CP2]} | Rest], Acc0) ->
    Acc1 = Acc0#{CP1 => CP2},
    Acc2 = Acc1#{CP2 => CP1},
    canonical_equivalence_data(Rest, Acc2);

canonical_equivalence_data([_ | Rest], Acc) ->
    canonical_equivalence_data(Rest, Acc).


case_mapping_funs_ast(MappingProperties, Name, IdxName, DataName) ->
    [case_mapping_index_fun_ast(IdxName, MappingProperties)
    ,case_mapping_data_fun_ast(DataName, MappingProperties)
    ,case_mapping_fun_ast(Name, IdxName, DataName)].


case_mapping_index_fun_ast(Name, MappingProperties) ->
    index_fun_ast(Name, compact(MappingProperties)).


case_mapping_data_fun_ast(Name, MappingProperties) ->
    Data = [V || {_,V} <- MappingProperties],
    Size = required_bits(lists:max(Data)),
    data_fun_ast(Name, [<<V:Size>> || V <- Data]).


case_mapping_fun_ast(Name, IdxName, DataName) ->
    ?Q(["'@Name@'(CP) ->"
       ,"  case '@IdxName@'(CP) of"
       ,"    undefined -> undefined;"
       ,"    Idx       -> '@DataName@'(Idx)"
       ,"  end."]).


special_casing_funs_ast(Data, lower) ->
    Data1 = [{CP, V, Cond} || {CP, V, _, _, Cond} <- Data, V /= [CP]],
    special_casing_funs_ast_1(Data1
                             ,ucd_special_casing_lower
                             ,ucd_special_casing_lower_idx
                             ,ucd_special_casing_lower_data);

special_casing_funs_ast(Data, title) ->
    Data1 = [{CP, V, Cond} || {CP, _, V, _, Cond} <- Data, V /= [CP]],
    special_casing_funs_ast_1(Data1
                             ,ucd_special_casing_title
                             ,ucd_special_casing_title_idx
                             ,ucd_special_casing_title_data);

special_casing_funs_ast(Data, upper) ->
    Data1 = [{CP, V, Cond} || {CP, _, _, V, Cond} <- Data, V /= [CP]],
    special_casing_funs_ast_1(Data1
                             ,ucd_special_casing_upper
                             ,ucd_special_casing_upper_idx
                             ,ucd_special_casing_upper_data).

special_casing_funs_ast_1(Data, Name, IdxName, DataName) ->
    Data1 = special_casing_values(Data),
    [special_casing_index_fun_ast(IdxName, Data1)
    ,special_casing_data_fun_ast(DataName, Data1)
    ,special_casing_fun_ast(Name, IdxName, DataName)].


special_casing_index_fun_ast(Name, Data) ->
    index_fun_ast(Name, compact(Data)).


special_casing_data_fun_ast(Name, Data) ->
    Data1 = list_to_tuple([Vs || {_, Vs} <- Data]),
    ?Q(["'@Name@'(Index) ->"
        "    element(Index + 1, _@Data1@)."
       ]).


special_casing_fun_ast(Name, IdxName, DataName) ->
    ?Q(["'@Name@'(CP) ->"
       ," case '@IdxName@'(CP) of"
       ,"    undefined -> undefined;"
       ,"    Idx       -> '@DataName@'(Idx)"
       ," end."
       ]).


special_casing_values(Data) ->
    Data1 = lists:foldl(fun special_casing_values_1/2, #{}, Data),
    unicodedata_ucd:sort_by_codepoints(maps:to_list(Data1)).


special_casing_values_1({CP, V, Cond}, Acc) ->
    case maps:get(CP, Acc, undefined) of
        undefined ->
            Acc#{CP => [special_casing_value(Cond, V)]};
        Vs ->
            Acc#{CP => [special_casing_value(Cond,  V) | Vs]}
    end.


special_casing_value([Lang, Ctx], V) when is_binary(Lang), is_atom(Ctx) ->
    {{Lang, Ctx}, V};

special_casing_value([LangOrCtx], V) ->
    {LangOrCtx, V};

special_casing_value([], V) ->
    V.


numeric_index_fun_ast(NumericProperties) ->
    index_fun_ast(ucd_numeric_idx, compact(NumericProperties)).


numeric_data_fun_ast(NumericProperties) ->
    Numerics = [V || {_,V} <- NumericProperties],
    Data = list_to_tuple(Numerics),
    ?Q(["ucd_numeric_data(Index) ->"
        "    element(Index + 1, _@Data@)."
       ]).

numeric_fun_ast() ->
    ExtraValues = unicodedata_ucd:unihan_numeric_values(),
    SortedValues = unicodedata_ucd:sort_by_codepoints(ExtraValues),
    DefaultAST = binary_search_ast(SortedValues, ?Q("undefined")),
    ?Q(["ucd_numeric(CP) ->"
       ," case ucd_numeric_idx(CP) of"
       ,"    undefined ->"
       ,"       _@DefaultAST;"
       ,"    Idx ->"
       ,"        ucd_numeric_data(Idx)"
       ," end."
       ]).


decomposition_index_fun_ast(Data) ->
    index_fun_ast(ucd_decomposition_idx, compact(Data)).


decomposition_data_fun_ast(Data) ->
    Data1 = list_to_tuple([V || {_,V} <- Data]),
    ?Q(["ucd_decomposition_data(Index) ->"
        "    element(Index + 1, _@Data1@)."
       ]).


decomposition_fun_ast() ->
    ?Q(["ucd_decomposition(CP) ->"
       ," case ucd_decomposition_idx(CP) of"
       ,"    undefined -> undefined;"
       ,"    Idx       -> ucd_decomposition_data(Idx)"
       ," end."
       ]).


nfd_quick_check_fun_ast(NormalizationProperties) ->
    Data = [{V,no} || {V, nfd_quick_check_no} <- NormalizationProperties],
    normalization_quickcheck_fun_ast(ucd_nfd_quick_check, Data).


nfc_quick_check_fun_ast(NormalizationProperties) ->
    Vs = lists:filtermap(fun ({V, nfc_quick_check_no})    -> {true, {V,no}};
                             ({V, nfc_quick_check_maybe}) -> {true, {V,maybe}};
                             (_)                          -> false
                         end, NormalizationProperties),
    Data = unicodedata_ucd:sort_by_codepoints(Vs),
    normalization_quickcheck_fun_ast(ucd_nfc_quick_check, Data).


nfkd_quick_check_fun_ast(NormalizationProperties) ->
    Data = [{V, no} || {V, nfkd_quick_check_no} <- NormalizationProperties],
    normalization_quickcheck_fun_ast(ucd_nfkd_quick_check, Data).


nfkc_quick_check_fun_ast(NormalizationProperties) ->
    Vs = lists:filtermap(fun ({V, nfkc_quick_check_no})    -> {true,{V,no}};
                             ({V, nfkc_quick_check_maybe}) -> {true,{V,maybe}};
                             (_)                           -> false
                         end, NormalizationProperties),
    Data = unicodedata_ucd:sort_by_codepoints(Vs),
    normalization_quickcheck_fun_ast(ucd_nfkc_quick_check, Data).


normalization_quickcheck_fun_ast(Name, Data) ->
    range_fun_ast(Name, range_values(Data), ?Q("yes")).


hangul_syllable_type_fun_ast() ->
    Data = unicodedata_ucd:hangul_syllable_type(),
    Rs = range_values(unicodedata_ucd:sort_by_codepoints(Data)),
    range_fun_ast(ucd_hangul_syllable_type, Rs, ?Q("not_applicable")).


block_fun_ast(Blocks) ->
    RangeValues = [{F,T,V} || {{F,T}, V} <- Blocks],
    range_fun_ast(ucd_block, RangeValues, ?Q("no_block")).


codepoint_range_fun_ast(Ranges) ->
    RangeValues = [codepoint_range_value(R) || R <- Ranges],
    range_fun_ast(ucd_codepoint_range, RangeValues).

codepoint_range_value(Range) ->
    {From, To} = element(1, Range),
    Name = unicodedata_ucd:range_name(element(2, Range)),
    {From, To, Name}.


east_asian_width_data_fun_ast() ->
    RangeValues = range_values(unicodedata_ucd:east_asian_width()),
    range_fun_ast(ucd_east_asian_width_data, RangeValues).

east_asian_width_defaults_fun_ast() ->
    DefaultValues = range_values(unicodedata_ucd:east_asian_width_defaults()),
    range_fun_ast(ucd_east_asian_width_defaults, DefaultValues, ?Q("neutral")).

east_asian_width_fun_ast() ->
    ?Q(["ucd_east_asian_width(CP) ->"
       ,"  case ucd_east_asian_width_data(CP) of"
       ,"    undefined -> ucd_east_asian_width_defaults(CP);"
       ,"    Value     -> Value"
       ,"  end."
       ]).


grapheme_break_fun_ast() ->
    Data = unicodedata_ucd:grapheme_break_property(),
    segmentation_fun_ast(ucd_grapheme_break, Data, other).


word_break_fun_ast() ->
    Data = unicodedata_ucd:word_break_property(),
    segmentation_fun_ast(ucd_word_break, Data, other).


sentence_break_fun_ast() ->
    Data = unicodedata_ucd:sentence_break_property(),
    segmentation_fun_ast(ucd_sentence_break, Data, other).


line_break_fun_ast() ->
    Data = unicodedata_ucd:line_break(),
    segmentation_fun_ast(ucd_line_break, Data, xx).


segmentation_fun_ast(Name, Data, Default) ->
    RangeValues = range_values(unicodedata_ucd:sort_by_codepoints(Data)),
    range_fun_ast(Name, RangeValues, ?Q("_@Default@")).


grapheme_break_classes_fun_ast(Name, Classes) ->
    Data = unicodedata_ucd:grapheme_break_property(),
    segmentation_classes_fun_ast(Name, Data, Classes).


word_break_classes_fun_ast(Name, Classes) ->
    Data = unicodedata_ucd:word_break_property(),
    segmentation_classes_fun_ast(Name, Data, Classes).


sentence_break_classes_fun_ast(Name, Classes) ->
    Data = unicodedata_ucd:sentence_break_property(),
    segmentation_classes_fun_ast(Name, Data, Classes).


line_break_classes_fun_ast(Name, Classes) ->
    Data = unicodedata_ucd:line_break(),
    segmentation_classes_fun_ast(Name, Data, Classes).


segmentation_classes_fun_ast(Name, Data, Classes) ->
    Data1 = unicodedata_ucd:sort_by_codepoints(Data),
    Data2 = [V || {V, Class} <- Data1, lists:member(Class, Classes)],
    Ranges = [{F, T, true} || {F, T} <- compact_ranges(Data2)],
    range_fun_ast(Name, Ranges, ?Q("false")).


name_aliases_fun_ast(Name, Type, Data) ->
    Data1 = lists:foldl(
              fun ({C, N, T}, Acc) when T == Type ->
                      maps:update_with(C, fun (V) -> V ++ [N] end, [N], Acc);
                  (_, Acc) ->
                      Acc
              end, #{}, Data),
    ?Q(["'@Name@'(CP) ->"
       ,"    maps:get(CP, _@Data1@, [])."
       ]).


codepoint_name_funs_ast(Data) ->
    Data1 = list_to_tuple([codepoint_name_data(CP) || CP <- Data]),
    [?Q(["ucd_codepoint_name(CP) ->"
        ,"  case ucd_properties_idx(CP) of"
        ,"   undefined -> undefined;"
        ,"   Idx       -> element(Idx+1, _@Data1@)"
        ,"  end."
        ])].

codepoint_name_data({_,<<C,_/binary>>=N,_,_,_,_,_,_,_,_,_}) when C /= $< ->
    N;
codepoint_name_data(_) ->
    undefined.


normalize_name_funs_ast() ->
    [?Q(["ucd_normalize_name(Name) ->"
        ,"    N1 = string:to_lower(binary_to_list(Name)),"
        ,"    N2 = string:tokens(N1, \" \"),"
        ,"    N3 = [ucd_normalize_name_1(N) || N <- N2],"
        ,"    iolist_to_binary(N3)."])
    ,?Q(["ucd_normalize_name_1(\"o-e\")   -> \"o-e\";"
        ,"ucd_normalize_name_1(\"g-o-e\") -> \"go-e\";"
        ,"ucd_normalize_name_1([A,$-,B | Rest]) when A >= $a, A =< $z,"
        ,"                                           B >= $a, B =< $z ->"
        ,"    [A | ucd_normalize_name_1([B|Rest])];"
        ,"ucd_normalize_name_1([H|T]) -> [H | ucd_normalize_name_1(T)];"
        ,"ucd_normalize_name_1([])    -> []."])
    ].

normalize_codepoint_name(Name) ->
    N1 = string:to_lower(binary_to_list(Name)),
    N2 = string:tokens(N1, " "),
    N3 = [normalize_name_1(N) || N <- N2],
    iolist_to_binary(N3).

% special case for U+1180 HANGUL JUNGSEONG O-E
normalize_name_1("o-e")   -> "o-e";
normalize_name_1("g-o-e") -> "go-e";
normalize_name_1([A,$-,B | Rest]) when A >= $a, A =< $z,
                                       B >= $a, B =< $z ->
    [A | normalize_name_1([B|Rest])];
normalize_name_1([H|T]) -> [H | normalize_name_1(T)];
normalize_name_1([])    -> [].


name_lookup_funs_ast(Name, Data, State) ->
    case name_lookup_data(Data) of
        {HashToCP, []} ->
            {[ name_lookup_fun_ast(Name, HashToCP) ], State};
        {HashToCP, Collisions} ->
            Forms = [ name_lookup_fun_ast(Name, HashToCP, Collisions) ],
            ensure_name_lookup_collision_fun_ast(Forms, State)
    end.

name_lookup_fun_ast(Name, HashToCP) ->
    {Data, Len} = name_lookup_hash_data(HashToCP),
    ?Q(["'@Name@'(Name) ->"
       ," Data = _@Data@,"
       ," Hash = erlang:phash2(ucd_normalize_name(Name)),"
       ," ucd_name_lookup(Hash, 0, _@Len@, Data)."
       ]).

name_lookup_fun_ast(Name, HashToCP, Collisions) ->
    {Data, Len} = name_lookup_hash_data(HashToCP),
    Cases = [?Q("_@H@ -> ucd_name_lookup_collision(N1,_@Cs@)") || {H,Cs} <- Collisions],
    Cases1 = Cases ++ [?Q("V -> ucd_name_lookup(V, 0, _@Len@, Data)")],
    ?Q(["'@Name@'(Name) ->"
       ," Data = _@Data@,"
       ," N1 = ucd_normalize_name(Name),"
       ," case erlang:phash2(N1) of"
       ,"   _ -> _@_@Cases1"
       ," end."
       ]).

name_lookup_collision_fun_ast() ->
    ?Q(["ucd_name_lookup_collision(_,[]) -> undefined;"
       ,"ucd_name_lookup_collision(N,[{CP, N} | _]) -> CP;"
       ,"ucd_name_lookup_collision(N,[_ | Cs])      -> ucd_name_lookup_collision(N,Cs)."
       ]).

name_lookup_search_fun() ->
    ?Q(["ucd_name_lookup(_, From, To, _) when From >= To -> undefined;"
       ,"ucd_name_lookup(Hash, From, To, Data) ->"
       ,"  Pos = From + (To - From) div 2,"
       ,"  case binary:part(Data, Pos*6, 6) of"
       ,"    <<Hash:27,CP:21>>           -> CP;"
       ,"    <<H:27,_:21>> when H > Hash -> ucd_name_lookup(Hash, From, Pos, Data);"
       ,"    _                           -> ucd_name_lookup(Hash, Pos+1, To, Data)"
       ,"  end."]).


name_lookup_codepoint_names(CPs) ->
    [{element(2,V), element(1,V)} || V <- CPs, binary:at(element(2,V),0) /= $<].


name_lookup_alias_names(Aliases, Types) ->
    [{Name, CP} || {CP, Name, Type} <- Aliases, lists:member(Type, Types)].


name_lookup_data(Data) ->
    ValuesMap = name_lookup_fold_data(Data, #{}),
    {HashToCP, Collisions} =
        maps:fold(fun (K, {CP, _}, {Acc1, Acc2}) ->
                          {[{K,CP} | Acc1], Acc2};
                      (K, Vs, {Acc1, Acc2}) ->
                          {Acc1, [{K,Vs} | Acc2]}
                  end, {[], []}, ValuesMap),
    {lists:keysort(1, HashToCP), Collisions}.


name_lookup_fold_data(Data, Acc) ->
    lists:foldl(fun ({Name, CP}, Acc0) ->
                        N = normalize_codepoint_name(Name),
                        V = {CP, N},
                        Hash = erlang:phash2(N),
                        case maps:get(Hash, Acc0, undefined) of
                            undefined -> Acc0#{Hash => V};
                            Vs when is_list(Vs)-> Acc0#{Hash => [V | Vs]};
                            V1 -> Acc0#{Hash => [V,V1]}
                        end
                end, Acc, Data).



name_lookup_hash_data(HashToCP) ->
    {<< <<H:27,V:21>> || {H,V} <- HashToCP >>, length(HashToCP)}.


decode_value_case_ast(ValueAST, Values) ->
    Cases = [?Q("_@V@ -> _@R@") || {R,V} <- enum_values(Values)],
    ?Q(["case _@ValueAST of"
       ," _ -> _@_@Cases"
       ,"end"
       ]).


binary_search_ast(Values, DefaultAST) ->
    binary_search_ast_1(split(Values), DefaultAST).

binary_search_ast_1({[], []}, DefaultAST) ->
    DefaultAST;

binary_search_ast_1({[{K, V}], []}, DefaultAST) ->
    ?Q(["if"
       ,"  CP == _@K@ -> _@V@;"
       ,"  true       -> _@DefaultAST"
       ,"end"]);

binary_search_ast_1({L1, [{K, V}|_]=L2}, DefaultAST) ->
    AST1 = binary_search_ast_1(split(L1), DefaultAST),
    AST2 = binary_search_ast_1(split(L2), DefaultAST),
    ?Q(["if"
       ,"  CP == _@K@ ->"
       ,"    _@V@;"
       ,"  CP < _@K@ ->"
       ,"    _@AST1;"
       ,"  true ->"
       ,"    _@AST2"
       ,"end"]).


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


compact(Data) ->
    compact_ranges([element(1,V) || V <- Data]).

compact_ranges([]) -> [];
compact_ranges([H|T]) ->
    case H of
        {Cp1, Cp2} -> compact_ranges(T, [{Cp1, Cp2}]);
        Cp         -> compact_ranges(T, [{Cp, Cp}])
    end.

compact_ranges([], Acc) ->
    lists:reverse(Acc);

compact_ranges([H|T], [{Cp1, Cp2}=Range | Acc]) ->
    case H of
        {NCp1, NCp2} ->
            compact_ranges(T, [{NCp1, NCp2}, Range | Acc]);
        Cp when Cp == Cp2 + 1 ->
            compact_ranges(T, [{Cp1, Cp} | Acc]);
        Cp ->
            compact_ranges(T, [{Cp, Cp}, Range | Acc])
    end.


range_values(Data) ->
    [case V of {{F,T},R} -> {F,T,R}; {Cp, R} -> {Cp, Cp, R} end || V <- Data].


get_file([]) ->
    "undefined";

get_file([Form | Forms]) ->
    case Form of
        ?Q("-file(\"'@File\",9090).") -> erl_syntax:string_value(File);
        _                             -> get_file(Forms)
    end.


error(File, Form, Fmt, Args) ->
    Ln = erl_syntax:get_pos(Form),
    throw({error, [{File, [{Ln, ?MODULE, {Fmt, Args}}]}], []}).


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

compact_test_() -> [
  ?_assertEqual([{0, 2}
                ,{4, 10}
                ,{15, 15}]
               ,compact([{0, a}
                        ,{1, b}
                        ,{2, c}
                        ,{{4,10}, d}
                        ,{15, e}]))
].

index_fun_ast_test_() ->
  Funs = [
    index_fun_ast(idx1,[{0,1}, {10,10}, {15,20}])
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
    range_fun_ast(range1,[{0,1,a}, {10,10,b}, {15,20,c}])
   ,range_fun_ast(range2,[{15,20,c}], ?Q("default"))
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
    data_fun_ast(f5,[<<1:5>>, <<2:5>>, <<3:5>>, <<4:5>>])
   ,data_fun_ast(f8,[<<1:8>>, <<2:8>>, <<3:8>>, <<4:8>>])
   ,data_fun_ast(f14,[<<1:14>>, <<2:14>>, <<3:14>>, <<4:14>>])
   ,data_fun_ast(fv,[<<1:5>>, <<2:5>>, <<0:5>>],
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

binary_search_ast_test_() ->
  S1 = binary_search_ast([{0,a}, {10,b}, {15,c}], ?Q("default")),
  Funs = [
    ?Q("f1(CP) -> _@S1.")
  ],
  {setup, fun() -> test_mod(ucd_binary_search, Funs) end, fun code:purge/1, [
     ?_assertEqual(a, ucd_binary_search:f1(0))
    ,?_assertEqual(b, ucd_binary_search:f1(10))
    ,?_assertEqual(c, ucd_binary_search:f1(15))
    ,?_assertEqual(default, ucd_binary_search:f1(5))
    ,?_assertEqual(default, ucd_binary_search:f1(25))
  ]}.

normalize_name_test_() ->
  Funs = normalize_name_funs_ast(),
  {setup, fun() -> test_mod(ucd_normalize_name, Funs) end, fun code:purge/1, [
     ?_assertEqual(<<"abcd">>,
                   ucd_normalize_name:ucd_normalize_name(<<"Ab  cD">>))

    ,?_assertEqual(<<"tibetanmarktsa-phru">>,
                   ucd_normalize_name:ucd_normalize_name(<<"TIBETAN MARK TSA -PHRU">>))

    ,?_assertEqual(ucd_normalize_name:ucd_normalize_name(<<"zero-width space">>),
                   ucd_normalize_name:ucd_normalize_name(<<"ZERO WIDTH SPACE">>))

    ,?_assertEqual(ucd_normalize_name:ucd_normalize_name(<<"ZERO WIDTH SPACE">>),
                   ucd_normalize_name:ucd_normalize_name(<<"zerowidthspace">>))

    ,?_assertNotEqual(ucd_normalize_name:ucd_normalize_name(<<"character -a">>),
                      ucd_normalize_name:ucd_normalize_name(<<"character a">>))
  ]}.


test_mod(Mod, Funs) ->
    Forms = [?Q("-module('@Mod@')."),
             ?Q("-compile(export_all).")
             | Funs
            ],
    merl:compile_and_load(Forms),
    Mod.

-endif.
