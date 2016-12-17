-module(unicodedata_ucd_transform).
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
                                     fun unicodedata_ucd:categories/0);
        ?Q("ucd_has_property(_@CP, _@V)") ->
            collect_ucd_property_fun(AST, CP, V, UcdFuns,
                                     ucd_has_property,
                                     fun unicodedata_ucd:prop_list_types/0);
        ?Q("ucd_name_aliases(_@CP, _@V)") ->
            collect_ucd_property_fun(AST, CP, V, UcdFuns,
                                     ucd_name_aliases,
                                     fun unicodedata_ucd:name_aliases_types/0);

        ?Q("ucd_grapheme_break(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                    ucd_grapheme_break,
                                    fun unicodedata_ucd:grapheme_break_classes/0);
        ?Q("ucd_word_break(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                    ucd_word_break,
                                    fun unicodedata_ucd:word_break_classes/0);
        ?Q("ucd_sentence_break(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                    ucd_sentence_break,
                                    fun unicodedata_ucd:sentence_break_classes/0);
        ?Q("ucd_line_break(_@CP, _@V)") ->
            collect_ucd_category_fun(AST, CP, V, UcdFuns,
                                    ucd_line_break,
                                    fun unicodedata_ucd:line_break_classes/0);
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

forms({ucd_bidi_mirroring_glyph, 1}, State) ->
    {[bidi_mirroring_glyph_fun_ast()], State};

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

forms({ucd_grapheme_break, Classes}, State) ->
    Name = ucd_fun_name(ucd_grapheme_break, Classes),
    {[grapheme_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_word_break, 1}, State) ->
    {[word_break_fun_ast()], State};

forms({ucd_word_break, Classes}, State) ->
    Name = ucd_fun_name(ucd_word_break, Classes),
    {[word_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_sentence_break, 1}, State) ->
    {[sentence_break_fun_ast()], State};

forms({ucd_sentence_break, Classes}, State) ->
    Name = ucd_fun_name(ucd_sentence_break, Classes),
    {[sentence_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_line_break, 1}, State) ->
    {[line_break_fun_ast()], State};

forms({ucd_line_break, Classes}, State) ->
    Name = ucd_fun_name(ucd_line_break, Classes),
    {[line_break_classes_fun_ast(Name, Classes)], State};

forms({ucd_is_category, Categories}, State) ->
    Name = ucd_fun_name(ucd_is_category, Categories),
    is_category_forms(Name, Categories, State);

forms({ucd_has_property, Property}, State) ->
    Name = ucd_fun_name(ucd_has_property, [Property]),
    has_property_forms(Name, Property, State);

forms({ucd_name_aliases, Type}, State0) ->
    Name = ucd_fun_name(ucd_name_aliases, [Type]),
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

forms({ucd_special_casing_upper, 1}, State) ->
    ucd_special_casing_forms(upper, State);

forms({ucd_special_casing_lower, 1}, State) ->
    ucd_special_casing_forms(lower, State);

forms({ucd_special_casing_title, 1}, State) ->
    ucd_special_casing_forms(title, State);

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
    {codepoint_name_funs_ast(Data), State1};

forms(_, State) ->
    {[], State}.


ensure_common_properties_index(Forms, State0) ->
    {Properties, State1} = common_properties_data(State0),
    case is_function_generated(ucd_properties_idx, State1) of
        true  ->
            {Forms, State1};
        false ->
            IdxFun = common_properties_index_fun_ast(Properties),
            {[IdxFun | Forms], function_generated(ucd_properties_idx, State1)}
    end.


ucd_special_casing_forms(Mapping, State0) ->
    {Data, State1} = special_casing_data(State0),
    {special_casing_funs_ast(Data, Mapping), State1}.


is_category_forms(Name, Categories, State0) ->
    {Data, State1} = ucd_data(State0),
    Forms = [is_category_fun_ast(Name, Data, Categories)],
    {Forms, State1}.


has_property_forms(Name, Property, State0) ->
    {Data, State1} = prop_list_data(State0),
    Forms = [has_property_fun_ast(Name, Data, Property)],
    {Forms, State1}.


is_function_generated(Name, #{generated_functions := Funs}) ->
    sets:is_element(Name, Funs).


function_generated(Name, #{generated_functions := Funs}=State) ->
    State#{generated_functions := sets:add_element(Name, Funs)}.


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


ucd_fun_name(Prefix, Parts) ->
    Suffix = string:join([atom_to_list(P) || P <- Parts], "_"),
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ Suffix).


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


bidi_mirroring_glyph_fun_ast() ->
    Data = unicodedata_ucd:bidi_mirroring(),
    Rs = range_values(unicodedata_ucd:sort_by_codepoints(Data)),
    range_fun_ast(ucd_bidi_mirroring_glyph, Rs, ?Q("none")).


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
    Data1 = [{CP, V, Cond} || {CP, V, _, _, Cond} <- Data, V /= CP],
    special_casing_funs_ast_1(Data1
                             ,ucd_special_casing_lower
                             ,ucd_special_casing_lower_idx
                             ,ucd_special_casing_lower_data);

special_casing_funs_ast(Data, title) ->
    Data1 = [{CP, V, Cond} || {CP, _, V, _, Cond} <- Data, V /= CP],
    special_casing_funs_ast_1(Data1
                             ,ucd_special_casing_title
                             ,ucd_special_casing_title_idx
                             ,ucd_special_casing_title_data);

special_casing_funs_ast(Data, upper) ->
    Data1 = [{CP, V, Cond} || {CP, _, _, V, Cond} <- Data, V /= CP],
    special_casing_funs_ast_1(Data1
                             ,ucd_special_casing_upper
                             ,ucd_special_casing_upper_idx
                             ,ucd_special_casing_upper_data).

special_casing_funs_ast_1(Data, Name, IdxName, DataName) ->
    Data1 = unicodedata_ucd:sort_by_codepoints(Data),
    [special_casing_index_fun_ast(IdxName, Data1)
    ,special_casing_data_fun_ast(DataName, Data1)
    ,special_casing_fun_ast(Name, IdxName, DataName)].


special_casing_index_fun_ast(Name, Data) ->
    index_fun_ast(Name, compact(Data)).


special_casing_data_fun_ast(Name, Data) ->
    Data1 = list_to_tuple([{V, Cond} || {_,V, Cond} <- Data]),
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
    Cases = lists:filtermap(fun codepoint_name_case_ast/1, Data),
    Cases1 = Cases ++ [?Q("_ -> undefined")],
    [?Q(["ucd_codepoint_name(CP) ->"
        ,"  case CP of"
        ,"   _ -> _@_@Cases1"
        ,"  end."
        ])].

codepoint_name_case_ast({CP,<<C,_/binary>>=N,_,_,_,_,_,_,_,_,_}) when C /= $< ->
    {true, ?Q("_@CP@ -> _@N@")};
codepoint_name_case_ast(_) ->
    false.


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


test_mod(Mod, Funs) ->
    Forms = [?Q("-module('@Mod@')."),
             ?Q("-compile(export_all).")
             | Funs
            ],
    merl:compile_and_load(Forms),
    Mod.

-endif.
