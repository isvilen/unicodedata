-module(unicodedata_bidirectional).
-compile({parse_transform, unicodedata_ucd_transform}).
-export([ bidirectional_class/1
        , is_mirrored/1
        , mirroring_glyph/1
        , bracket/1
        , paragraph/1
        , paragraph/2
        , embedding_level/1
        , embedding_levels/1
        , embedding_levels/2
        , embedding_level_kind/1
        , reorder_indices/1
        , reorder_indices/2
        , reorder/1
        , reorder/2
        ]).

-export_type([ bidirectional_class/0
             , paragraph/0
             ]).


-type bidirectional_class() :: left_to_right
                             | right_to_left
                             | arabic_letter

                             | european_number
                             | european_separator
                             | european_terminator
                             | arabic_number
                             | common_separator
                             | nonspacing_mark
                             | boundary_neutral

                             | paragraph_separator
                             | segment_separator
                             | white_space
                             | other_neutral

                             | left_to_right_embedding
                             | left_to_right_override
                             | right_to_left_embedding
                             | right_to_left_override
                             | pop_directional_format
                             | left_to_right_isolate
                             | right_to_left_isolate
                             | first_strong_isolate
                             | pop_directional_isolate.


-spec bidirectional_class(char()) -> bidirectional_class().
bidirectional_class(CP) ->
    ucd_bidi_class(CP).


-spec is_mirrored(char()) -> boolean().
is_mirrored(CP) ->
    ucd_bidi_mirrored(CP).


-spec mirroring_glyph(char()) -> char() | none.
mirroring_glyph(CP) ->
    ucd_bidi_mirroring_glyph(CP).


-spec bracket(char()) -> {open, char()} | {close, char()} | none.
bracket(CP) ->
    ucd_bidi_brackets(CP).


-type array(T) :: array:array(T).

-opaque paragraph() :: { array(char()) % codepoints
                       , array(bidirectional_class()) % types
                       , non_neg_integer() % paragraph embedding level
                       , array(non_neg_integer()) % levels
                       }.


-spec paragraph(string()) -> paragraph().
paragraph(String) ->
    CPs = codepoints(String),
    Types = initial_types(String),
    {MPDI, MII} = determine_matching_isolates(Types),
    Level = determine_paragraph_embedding_level(Types, MPDI),
    run_uba(CPs, Types, Level, MPDI, MII).


-spec paragraph(string(), EmbeddingLevel) -> paragraph()
      when  EmbeddingLevel :: non_neg_integer().
paragraph(String, EmbeddingLevel) ->
    CPs = codepoints(String),
    Types = initial_types(String),
    {MPDI, MII} = determine_matching_isolates(Types),
    run_uba(CPs, Types, EmbeddingLevel, MPDI, MII).


-spec embedding_level(paragraph()) -> non_neg_integer().
embedding_level({_, _, Level, _}) -> Level.


-spec embedding_levels(paragraph()) -> [non_neg_integer() | hide].
embedding_levels(Paragraph) ->
    embedding_levels(Paragraph, []).


-spec embedding_levels(paragraph(), Options) -> [non_neg_integer() | hide]
      when Options :: [ {line_breaks, [non_neg_integer()]}
                      | {hide_explicit_directional_formatting, boolean()} ].
embedding_levels({CPs, Types, ParLevel, Levels}, Options) ->
    LineBreaks = line_breaks(CPs, Options),
    Levels1 = get_levels(Levels, Types, ParLevel, LineBreaks),
    case hide_explicit_directional_formatting(Options) of
        true  -> hide_levels_for_characters_removed_by_X9(Types, Levels1);
        false -> array:to_list(Levels1)
    end.


-spec embedding_level_kind(Level) -> left_to_right | right_to_left
      when Level :: non_neg_integer().
embedding_level_kind(Level) when is_integer(Level) ->
    type_for_level(Level).


-spec reorder_indices(paragraph()) -> [non_neg_integer()].
reorder_indices(Paragraph) ->
    reorder_indices(Paragraph, []).


-spec reorder_indices(paragraph(), Options) -> [non_neg_integer()]
      when Options :: [ {line_breaks, [non_neg_integer()]}
                      | {hide_explicit_directional_formatting, boolean()} ].
reorder_indices({CPs, Types, ParLevel, Levels}, Options) ->
    LineBreaks = line_breaks(CPs, Options),
    Levels1 = get_levels(Levels, Types, ParLevel, LineBreaks),
    Indices = multiline_reordering(Levels1, LineBreaks),
    case hide_explicit_directional_formatting(Options) of
        true ->
            [Idx || Idx <- Indices, not is_removed_by_X9(array:get(Idx, Types))];
        false ->
            Indices
    end.


-spec reorder(paragraph()) -> string().
reorder(Paragraph) ->
    reorder(Paragraph, []).


-spec reorder(paragraph(), Options) -> string()
      when Options :: [ {line_breaks, [non_neg_integer()]}
                      | {hide_explicit_directional_formatting, boolean()} ].
reorder(Paragraph, Options) ->
    CPs = element(1, Paragraph),
    Indices = reorder_indices(Paragraph, Options),
    [array:get(Idx,CPs) || Idx <- Indices].


line_breaks(CPs, Options) ->
    case proplists:get_value(line_breaks, Options) of
        undefined -> [array:size(CPs)];
        Breaks    -> validate_line_breaks(Breaks, 0, array:size(CPs)), Breaks
    end.


validate_line_breaks([Break], _, Break) ->
    ok;

validate_line_breaks([Break | Breaks], Prev, Last) when Break > Prev ->
    validate_line_breaks(Breaks, Break, Last);

validate_line_breaks(_, _, _) ->
    error({badarg, line_breaks}).


hide_explicit_directional_formatting(Options) ->
    proplists:get_value(hide_explicit_directional_formatting, Options, true).


codepoints(String) ->
    array:from_list(String).


initial_types(String) ->
    array:from_list([ucd_bidi_class(CP) || CP <- String]).


determine_matching_isolates(Types) ->
    Size = array:size(Types),
    MPDI = array:new(Size, [{default, undefined}]),
    MII = array:new(Size, [{default, undefined}]),
    determine_matching_isolates(0, array:size(Types), Types, MPDI, MII).

determine_matching_isolates(Idx, Idx, _, MPDI, MII) ->
    {MPDI, MII};

determine_matching_isolates(Idx, ToIdx, Types, MPDI, MII) ->
    case array:get(Idx, Types) of
        Type when Type == first_strong_isolate
                ; Type == left_to_right_isolate
                ; Type == right_to_left_isolate ->
            determine_matching_isolates_1(Idx, Idx + 1, ToIdx, 1, Types, MPDI, MII);
        _ ->
            determine_matching_isolates(Idx + 1, ToIdx, Types, MPDI, MII)
    end.


determine_matching_isolates_1(StartIdx, ToIdx, ToIdx, _, Types, MPDI, MII) ->
    MPDI1 = array:set(StartIdx, ToIdx, MPDI),
    determine_matching_isolates(StartIdx + 1, ToIdx, Types, MPDI1, MII);

determine_matching_isolates_1(StartIdx, Idx, ToIdx, Depth, Types, MPDI, MII) ->
    case array:get(Idx, Types) of
        Type when Type == first_strong_isolate
                ; Type == left_to_right_isolate
                ; Type == right_to_left_isolate ->
            determine_matching_isolates_1(StartIdx, Idx + 1, ToIdx, Depth + 1, Types, MPDI, MII);

        pop_directional_isolate when Depth > 1 ->
            determine_matching_isolates_1(StartIdx, Idx + 1, ToIdx, Depth - 1, Types, MPDI, MII);

        pop_directional_isolate ->
            MPDI1 = array:set(StartIdx, Idx, MPDI),
            MII1 = array:set(Idx, StartIdx, MII),
            determine_matching_isolates(StartIdx + 1, ToIdx, Types, MPDI1, MII1);

        _ ->
            determine_matching_isolates_1(StartIdx, Idx + 1, ToIdx, Depth, Types, MPDI, MII)
    end.


determine_paragraph_embedding_level(Types, MatchingPDI) ->
    find_embedding_level(Types, 0, array:size(Types), MatchingPDI).


find_embedding_level(_, Idx, Idx, _) ->
    0;

find_embedding_level(Types, Idx, ToIdx, MatchingPDI) ->
    case array:get(Idx, Types) of
        left_to_right ->
            0;

        Type when Type == right_to_left
                ; Type == arabic_letter ->
            1;

        Type when Type == first_strong_isolate
                ; Type == left_to_right_isolate
                ; Type == right_to_left_isolate ->
            find_embedding_level(Types, array:get(Idx, MatchingPDI), ToIdx, MatchingPDI);

        _ ->
            find_embedding_level(Types, Idx + 1, ToIdx, MatchingPDI)
    end.


run_uba(CPs, Types, ParLevel, MPDI, MII) ->
    {Types1, Levels} = determine_explicit_embedding_levels(Types, ParLevel, MPDI),
    Levels1 = process_isolating_run_sequences(CPs, Types, Types1, ParLevel, Levels, MPDI, MII),
    Levels2 = assign_levels_to_characters_removed_by_X9(Types, Levels1, ParLevel),
    Levels3 = finalize_levels(Levels2, Types, ParLevel),
    {CPs, Types, ParLevel, Levels3}.


% directional status state
-record(ds_state,{ types
                 , levels
                 , default_level
                 , matching_pdi
                 , counter = 0
                 , embedding_level = []
                 , override_status = []
                 , isolate_status = []
                 , overflow_isolate_count = 0
                 , overflow_embedding_count = 0
                 , valid_isolate_count = 0
                 }).

-define(MAX_DEPTH,125).

-define(CHECK_LEVEL(L,S),L =< ?MAX_DEPTH
                        ,S#ds_state.overflow_isolate_count == 0
                        ,S#ds_state.overflow_embedding_count == 0).


determine_explicit_embedding_levels(Types, DefaultLevel, MatchingPDI) ->
    Levels = array:new(array:size(Types), [{default, DefaultLevel}]),
    S0 = #ds_state{ types=Types
                  , levels=Levels
                  , default_level=DefaultLevel
                  , matching_pdi=MatchingPDI
                  },
    S1 = ds_push(DefaultLevel, other_neutral, false, S0),
    determine_explicit_embedding_levels_1(0, array:size(Types), S1).


determine_explicit_embedding_levels_1(Idx, Idx, #ds_state{types=Ts,levels=Ls}) ->
    {Ts, Ls};

determine_explicit_embedding_levels_1(Idx, LastIdx, S0) ->
    Type = array:get(Idx, S0#ds_state.types),
    S1 = determine_explicit_embedding_levels_2(Idx, Type, S0),
    determine_explicit_embedding_levels_1(Idx + 1, LastIdx, S1).


determine_explicit_embedding_levels_2(Idx, Type, S0)
  when Type == right_to_left_isolate
     ; Type == left_to_right_isolate
     ; Type == first_strong_isolate ->

    S1 = case ds_last_override_status(S0) of
             other_neutral      -> S0;
             LastOverrideStatus -> ds_set_type(Idx, LastOverrideStatus, S0)
         end,

    S2 = ds_set_last_embedding_level(Idx, S1),

    case ds_next_embedding_level(Idx, Type, S0) of
        Level when ?CHECK_LEVEL(Level,S0) ->
            S3 = ds_push(Level, Type, true, S2),
            ds_add_valid_isolate_count(S3);
        _ ->
            ds_add_overflow_isolate_count(S2)
    end;

determine_explicit_embedding_levels_2(Idx, Type, S0)
  when Type == right_to_left_embedding
     ; Type == left_to_right_embedding
     ; Type == right_to_left_override
     ; Type == left_to_right_override ->

    case ds_next_embedding_level(Idx, Type, S0) of
        Level when ?CHECK_LEVEL(Level,S0) ->
            S1 = ds_push(Level, Type, false, S0),
            ds_set_embedding_level(Idx, Level, S1);
        _ ->
            case ds_set_last_embedding_level(Idx, S0) of
                S1 when S1#ds_state.overflow_isolate_count == 0 ->
                    ds_add_overflow_embedding_count(S1);
                S1 -> S1
            end
    end;

determine_explicit_embedding_levels_2(Idx, pop_directional_isolate, S0) ->
    S1 = if
             S0#ds_state.overflow_isolate_count > 0 ->
                 ds_sub_overflow_isolate_count(S0);

             S0#ds_state.valid_isolate_count == 0 ->
                 S0;

             true ->
                 ds_pop_directional_isolate(S0#ds_state{overflow_embedding_count=0})
         end,
    ds_set_last_embedding_level(Idx, S1);

determine_explicit_embedding_levels_2(Idx, pop_directional_format, S0) ->
    S1 = ds_set_last_embedding_level(Idx, S0),
    if
        S1#ds_state.overflow_isolate_count > 0 ->
            S1;

        S1#ds_state.overflow_embedding_count > 0 ->
            ds_sub_overflow_embedding_count(S1);

        true ->
            case ds_last_isolate_status(S1) of
                false when S1#ds_state.counter >= 2 -> ds_pop(S1);
                _                                   -> S1
            end
    end;

determine_explicit_embedding_levels_2(Idx, paragraph_separator, State) ->
    Level = State#ds_state.default_level,
    State#ds_state{levels = array:set(Idx, Level, State#ds_state.levels)};

determine_explicit_embedding_levels_2(Idx, _, S0) ->
    S1 = ds_set_last_embedding_level(Idx, S0),
    case ds_last_override_status(S1) of
        other_neutral      -> S1;
        LastOverrideStatus -> ds_set_type(Idx, LastOverrideStatus, S1)
    end.


ds_push(Level, OverrideType, IsolateStatus, #ds_state{ counter=Counter
                                                     , embedding_level = Ls
                                                     , override_status = OSs
                                                     , isolate_status = ISs
                                                     }=S0) ->
    OverrideStatus = case OverrideType of
                         left_to_right_override -> left_to_right;
                         right_to_left_override -> right_to_left;
                         _                      -> other_neutral
                     end,
    S0#ds_state{ counter = Counter + 1
               , embedding_level = [Level | Ls]
               , override_status = [OverrideStatus | OSs]
               , isolate_status = [IsolateStatus | ISs]
               }.


ds_pop(#ds_state{ counter=Counter
                , embedding_level = [_ | Ls]
                , override_status = [_ | OSs]
                , isolate_status = [_ | ISs]
                }=S0) ->

    S0#ds_state{ counter = Counter - 1
               , embedding_level = Ls
               , override_status = OSs
               , isolate_status = ISs
               }.

ds_set_last_embedding_level(Idx, #ds_state{levels=Levels,embedding_level=Ls}=S0) ->
    S0#ds_state{levels=array:set(Idx, hd(Ls), Levels)}.


ds_last_embedding_level(#ds_state{embedding_level=[L|_]}) -> L.


ds_set_embedding_level(Idx, Level, #ds_state{levels=Levels}=S0) ->
    S0#ds_state{levels=array:set(Idx, Level, Levels)}.


ds_pop_directional_isolate(#ds_state{isolate_status=[false|_]}=S0) ->
    ds_pop_directional_isolate(ds_pop(S0));

ds_pop_directional_isolate(S0) ->
    S1 = ds_pop(S0),
    S1#ds_state{valid_isolate_count = S1#ds_state.valid_isolate_count - 1}.


ds_next_embedding_level(Idx, Type, State) ->
    MatchingPDI = State#ds_state.matching_pdi,
    IsRTL = case Type of
                first_strong_isolate ->
                    find_embedding_level(State#ds_state.types
                                        ,Idx + 1
                                        ,array:get(Idx, MatchingPDI)
                                        ,MatchingPDI
                                        ) == 1;
                right_to_left_embedding -> true;
                right_to_left_override  -> true;
                right_to_left_isolate   -> true;
                _                       -> false
            end,
    case IsRTL of
        true -> % least greater odd
            (ds_last_embedding_level(State) + 1) bor 1;

        false -> % least greater even
            (ds_last_embedding_level(State) + 2) band (bnot 1)
    end.


ds_add_valid_isolate_count(#ds_state{valid_isolate_count=V}=S) ->
    S#ds_state{valid_isolate_count = V + 1}.


ds_add_overflow_isolate_count(#ds_state{overflow_isolate_count=V}=S) ->
    S#ds_state{overflow_isolate_count = V + 1}.

ds_sub_overflow_isolate_count(#ds_state{overflow_isolate_count=V}=S) ->
    S#ds_state{overflow_isolate_count = V - 1}.


ds_add_overflow_embedding_count(#ds_state{overflow_embedding_count=V}=S) ->
    S#ds_state{overflow_embedding_count = V + 1}.

ds_sub_overflow_embedding_count(#ds_state{overflow_embedding_count=V}=S) ->
    S#ds_state{overflow_embedding_count = V - 1}.


ds_last_isolate_status(#ds_state{isolate_status=[V|_]}) -> V.


ds_last_override_status(#ds_state{override_status=[V|_]}) -> V.


ds_set_type(Idx, Type, #ds_state{types=Types}=State) ->
    State#ds_state{types=array:set(Idx, Type, Types)}.


%% definition BD13
process_isolating_run_sequences(CPs, InitialTypes, Types, ParLevel, Levels, MPDI, MII) ->
    LevelRuns = determine_level_runs(InitialTypes, Levels),
    RunForCh = run_for_character(LevelRuns),
    array:foldl(fun (_, Run, Acc) ->
                    case isolating_run(Run, LevelRuns, RunForCh, InitialTypes, MPDI, MII) of
                        undefined ->
                            Acc;
                        Run1 ->
                            process_isolating_run_sequence(Run1, CPs, Types, InitialTypes, ParLevel, Levels, Acc)
                    end
                end, Levels, LevelRuns).


isolating_run([FirstCh|_]=Run, LevelRuns, RunForCh, InitialTypes, MPDI, MII) ->
    case array:get(FirstCh, InitialTypes) of
        T when T /= pop_directional_isolate ->
            isolating_run_1(Run, LevelRuns, RunForCh, InitialTypes, MPDI);
        _ ->
            case array:get(FirstCh, MII) of
                undefined ->
                    isolating_run_1(Run, LevelRuns, RunForCh, InitialTypes, MPDI);
                _ ->
                    undefined
            end
    end.


isolating_run_1(Run, LevelRuns, RunForCh, InitialTypes, MPDI) ->
    LastCh = lists:last(Run),
    case array:get(LastCh, InitialTypes) of
        T when T == left_to_right_isolate
             ; T == right_to_left_isolate
             ; T == first_strong_isolate ->
            MatchingPDI = array:get(LastCh, MPDI),
            TextLength = array:size(InitialTypes),
            if
                MatchingPDI /= TextLength ->
                    NextRunIdx = maps:get(MatchingPDI, RunForCh),
                    NextRun = Run ++ array:get(NextRunIdx, LevelRuns),
                    isolating_run_1(NextRun, LevelRuns, RunForCh, InitialTypes, MPDI);
                true ->
                    Run
            end;
        _ ->
            Run
    end.


prev_level([Idx | _], Types, InitialTypes, ParLevel, Levels) ->
    prev_level_1(Idx - 1, Types, InitialTypes, ParLevel, Levels).

prev_level_1(Idx, Types, InitialTypes, ParLevel, Levels) when Idx >= 0 ->
    case is_removed_by_X9(array:get(Idx, InitialTypes)) of
        true  -> prev_level_1(Idx - 1, Types, InitialTypes, ParLevel, Levels);
        false -> array:get(Idx, Levels)
    end;

prev_level_1(_, _, _, ParLevel, _) ->
    ParLevel.


succ_level(Run, Types, InitialTypes, ParLevel, Levels) ->
    LastIdx = lists:last(Run),
    case array:get(LastIdx, Types) of
        Type when Type == left_to_right_isolate
                ; Type == right_to_left_isolate
                ; Type == first_strong_isolate ->
            ParLevel;
        _ ->
            succ_level_1(LastIdx + 1, InitialTypes, ParLevel, Levels)
    end.

succ_level_1(Idx, InitialTypes, ParLevel, Levels) ->
    Size = array:size(InitialTypes),
    if
        Idx >= Size ->
            ParLevel;
        true ->
            case is_removed_by_X9(array:get(Idx, InitialTypes)) of
                true  -> succ_level_1(Idx+1, InitialTypes, ParLevel, Levels);
                false -> array:get(Idx, Levels)
            end
    end.


determine_level_runs(Types, Levels) ->
    determine_level_runs(0, array:size(Types), Types, Levels, hide, [[]]).


determine_level_runs(Idx, Idx, _, _, _, Runs) ->
    Runs1 = case Runs of
                [[] | Rs] -> Rs;
                _         -> Runs
            end,
    array:from_list(lists:reverse([lists:reverse(R) || R <- Runs1]));

determine_level_runs(Idx, LastIdx, Types, Levels, CurrentLevel, [R|Rs]=Runs) ->
    {CurrentLevel1, Runs1} =
        case is_removed_by_X9(array:get(Idx, Types)) of
            true ->
                {CurrentLevel, Runs};
            false ->
                case array:get(Idx, Levels) of
                    CurrentLevel ->
                        {CurrentLevel, [[Idx | R] | Rs]};
                    Level when R == [] ->
                        {Level, [[Idx] | Rs]};
                    Level ->
                        {Level, [[Idx] | Runs]}
                end
        end,
    determine_level_runs(Idx+1, LastIdx, Types, Levels, CurrentLevel1, Runs1).


run_for_character(LevelRuns) ->
    array:foldl(fun run_for_character/3, #{}, LevelRuns).

run_for_character(RunIdx, LevelRun, AccIn) ->
    lists:foldl(fun (ChIdx, Acc) -> Acc#{ChIdx => RunIdx} end, AccIn, LevelRun).


process_isolating_run_sequence(Run, CPs, Types, InitialTypes, ParLevel, LevelsIn, LevelsOut) ->
    Indices = array:from_list(Run),

    Level = array:get(hd(Run), LevelsIn),

    PrevLevel = prev_level(Run, Types, InitialTypes, ParLevel, LevelsIn),
    SuccLevel = succ_level(Run, Types, InitialTypes, ParLevel, LevelsIn),

    SoS = type_for_level(max(PrevLevel, Level)),
    EoS = type_for_level(max(SuccLevel, Level)),

    Ts0 = array:from_list([array:get(Idx, Types) || Idx <- Run]),

    % Rules W1-W7
    Ts1 = resolve_weak_types(Ts0, SoS, EoS),

    % Rule N0
    Ts2 = resolve_paired_brackets(Ts1, Level, Indices, SoS, CPs, InitialTypes),

    % Rules N1-N3
    Ts3 = resolve_neutral_types(Ts2, Level, SoS, EoS),

    % Rules I1, I2
    resolve_implicit_levels(Ts3, Level, Indices, LevelsOut).


resolve_weak_types(Ts0, SoS, EoS) ->
    Ts1 = resolve_weak_types_w1(Ts0, SoS),
    Ts2 = resolve_weak_types_w2(Ts1),
    Ts3 = resolve_weak_types_w3(Ts2),
    Ts4 = resolve_weak_types_w4(Ts3),
    Ts5 = resolve_weak_types_w5(Ts4, SoS, EoS),
    Ts6 = resolve_weak_types_w6(Ts5),
    resolve_weak_types_w7(Ts6, SoS).


% Rule W1: Changes all NSMs
resolve_weak_types_w1(Ts, SoS) ->
    resolve_weak_types_w1(0, array:size(Ts), Ts, SoS).

resolve_weak_types_w1(Idx, Idx, Types, _) ->
    Types;

resolve_weak_types_w1(Idx, Size, Types, PrevCharType) ->
    case array:get(Idx, Types) of
        nonspacing_mark ->
            Types1 = array:set(Idx, PrevCharType, Types),
            resolve_weak_types_w1(Idx + 1, Size, Types1, PrevCharType);
        T when T == left_to_right_isolate
             ; T == right_to_left_isolate
             ; T == first_strong_isolate
             ; T == pop_directional_isolate ->
            resolve_weak_types_w1(Idx + 1, Size, Types, other_neutral);
        T ->
            resolve_weak_types_w1(Idx + 1, Size, Types, T)
    end.


% Rule W2: EN does not change at the start of the run, because sos != AL
resolve_weak_types_w2(Ts) ->
    resolve_weak_types_w2(0, array:size(Ts), Ts).

resolve_weak_types_w2(Idx, Idx, Types) ->
    Types;

resolve_weak_types_w2(Idx, Size, Types) ->
    case array:get(Idx, Types) of
        european_number ->
            Types1 = resolve_weak_types_w2_1(Idx, Idx - 1, Types),
            resolve_weak_types_w2(Idx + 1, Size, Types1);
        _ ->
            resolve_weak_types_w2(Idx + 1, Size, Types)
    end.

resolve_weak_types_w2_1(Idx0, Idx, Types) when Idx >= 0 ->
    case array:get(Idx, Types) of
        T when T == left_to_right
             ; T == right_to_left ->
            Types;
        arabic_letter ->
            array:set(Idx0, arabic_number, Types);
        _ ->
            resolve_weak_types_w2_1(Idx0, Idx - 1, Types)
    end;

resolve_weak_types_w2_1(_, _, Types) ->
    Types.


% Rule W3
resolve_weak_types_w3(Ts) ->
    resolve_weak_types_w3(0, array:size(Ts), Ts).

resolve_weak_types_w3(Idx, Idx, Types) ->
    Types;

resolve_weak_types_w3(Idx, Size, Types) ->
    case array:get(Idx, Types) of
        arabic_letter ->
            Types1 = array:set(Idx, right_to_left, Types),
            resolve_weak_types_w3(Idx + 1, Size, Types1);
        _ ->
            resolve_weak_types_w3(Idx + 1, Size, Types)
    end.


% Rule W4
resolve_weak_types_w4(Ts) ->
    resolve_weak_types_w4(1, array:size(Ts) - 1, Ts).


resolve_weak_types_w4(Idx, End, Types) when Idx >= End ->
    Types;

resolve_weak_types_w4(Idx, End, Types) ->
    case array:get(Idx, Types) of
        T when T == european_separator
             ; T == common_separator ->
            Types1 =
                case {array:get(Idx - 1, Types), array:get(Idx + 1, Types)} of
                    {european_number, european_number} ->
                        array:set(Idx, european_number, Types);
                    {arabic_number, arabic_number} when T == common_separator ->
                        array:set(Idx, arabic_number, Types);
                    _ ->
                        Types
                end,
            resolve_weak_types_w4(Idx + 1, End, Types1);
        _ ->
            resolve_weak_types_w4(Idx + 1, End, Types)
    end.


% Rule W5
resolve_weak_types_w5(Ts, SoS, EoS) ->
    resolve_weak_types_w5(0, array:size(Ts), Ts, SoS, EoS).

resolve_weak_types_w5(Idx, Idx, Types, _, _) ->
    Types;

resolve_weak_types_w5(Idx, Size, Types, SoS, EoS) ->
    case array:get(Idx, Types) of
        european_terminator ->
            RunStart = Idx,
            RunLimit = find_run_limit(RunStart, Size, Types, [european_terminator]),

            T0 = case RunStart of
                     0 -> SoS;
                     _ -> array:get(RunStart - 1, Types)
                 end,
            T1 = if
                     T0 /= european_number ->
                         case RunLimit of
                             Size -> EoS;
                             _    -> array:get(RunLimit, Types)
                         end;
                     true -> T0
                 end,

            Types1 = case T1 of
                         european_number ->
                             set_types(RunStart, RunLimit, european_number, Types);
                         _ ->
                             Types
                     end,
            resolve_weak_types_w5(RunLimit, Size, Types1, SoS, EoS);

        _ ->
            resolve_weak_types_w5(Idx + 1, Size, Types, SoS, EoS)
    end.


% Rule W6
resolve_weak_types_w6(Ts) ->
    resolve_weak_types_w6(0, array:size(Ts), Ts).

resolve_weak_types_w6(Idx, Idx, Types) ->
    Types;

resolve_weak_types_w6(Idx, Size, Types) ->
    case array:get(Idx, Types) of
        T when T == european_separator
             ; T == european_terminator
             ; T == common_separator ->
            Types1 = array:set(Idx, other_neutral, Types),
            resolve_weak_types_w6(Idx + 1, Size, Types1);
        _ ->
            resolve_weak_types_w6(Idx + 1, Size, Types)
    end.


% Rule W7
resolve_weak_types_w7(Ts, SoS) ->
    resolve_weak_types_w7(0, array:size(Ts), Ts, SoS).

resolve_weak_types_w7(Idx, Idx, Types, _) ->
    Types;

resolve_weak_types_w7(Idx, Size, Types, SoS) ->
    case array:get(Idx, Types) of
        european_number ->
            Types1 = case resolve_weak_types_w7_1(Idx - 1, Types, SoS) of
                         left_to_right -> array:set(Idx, left_to_right, Types);
                         _             -> Types
                     end,
            resolve_weak_types_w7(Idx + 1, Size, Types1, SoS);
        _ ->
            resolve_weak_types_w7(Idx + 1, Size, Types, SoS)
    end.

resolve_weak_types_w7_1(Idx, Types, PrevStrongType) when Idx >= 0 ->
    case array:get(Idx, Types) of
        T when T == left_to_right
             ; T == right_to_left -> % // AL's have been changed to R
            T;
        _ ->
            resolve_weak_types_w7_1(Idx - 1, Types, PrevStrongType)
    end;

resolve_weak_types_w7_1(_, _, PrevStrongType) ->
    PrevStrongType.


-define(BRACKETS_STACK_LIMIT,63).

resolve_paired_brackets(Types0, Level, Indices, SoS, CPs, InitialTypes) ->
    Brackets = locate_brackets(Indices, Types0, CPs),
    resolve_brackets(Brackets, Types0, SoS, type_for_level(Level), Indices, InitialTypes).


locate_brackets(Indices, Types, CPs) ->
    Bs = try array:foldl(fun (Idx, V, Acc) ->
                                 locate_brackets_1(Idx, V, Types, CPs, Acc)
                         end, {[], [], 0}, Indices)
         of
             {Bs0, _, _} -> Bs0
         catch
             throw:{stop, Bs1} -> Bs1
         end,
    lists:sort(fun({S1,_}, {S2,_}) -> S1 =< S2 end, Bs).

locate_brackets_1(Idx, ChIdx, Types, CPs, Acc) ->
    Ch = array:get(ChIdx, CPs),
    case ucd_bidi_brackets(Ch) of
        none ->
            Acc;
        Bracket ->
            case array:get(Idx, Types) of
                other_neutral -> locate_brackets_2(Bracket, Idx, Ch, Acc);
                _             -> Acc
            end
    end.


locate_brackets_2({open, _}, Idx, Ch, {Brackets, Openers, StackSize}) ->
    case StackSize < ?BRACKETS_STACK_LIMIT of
        true  -> {Brackets, [{Idx, Ch} | Openers], StackSize + 1};
        false -> throw({stop, Brackets})
    end;

locate_brackets_2({close, Chs}, Idx, _Ch, {Brackets, Openers, StackSize}=Acc) ->
    case find_opener(Chs, Openers) of
        {OpenerIdx, Openers1} ->
            {[{OpenerIdx, Idx} | Brackets], Openers1, StackSize - 1};
        _ ->
            Acc
    end.


find_opener(_, []) ->
    undefined;

find_opener(Chs, [{Idx, Ch} | Openers]) ->
    case lists:member(Ch, Chs) of
        true  -> {Idx, Openers};
        false -> find_opener(Chs, Openers)
    end.


resolve_brackets(Brackets, Types, SoS, DirEmbed, Indices, InitialTypes) ->
    lists:foldl(fun (Bracket, T0) ->
                        assign_bracket_type(Bracket, T0, SoS, DirEmbed, Indices, InitialTypes)
                end, Types, Brackets).


assign_bracket_type(Bracket, Types, SoS, DirEmbed, Indices, InitialTypes) ->
    case classify_bracket_content(Bracket, Types, DirEmbed) of
        other_neutral ->
            Types;
        DirEmbed ->
            set_brackets_to_type(Bracket, DirEmbed, Types, Indices, InitialTypes);
        _ ->
            V = class_before_bracket(Bracket, Types, SoS),
            set_brackets_to_type(Bracket, V, Types, Indices, InitialTypes)
    end.


classify_bracket_content({OpenIdx, CloseIdx}, Types, DirEmbed) ->
    classify_bracket_content_1(OpenIdx+1, CloseIdx, Types, DirEmbed, other_neutral).

classify_bracket_content_1(Idx, Idx, _, _, DirOpposite) ->
    DirOpposite;

classify_bracket_content_1(Idx, CloseIdx, Types, DirEmbed, DirOpposite) ->
    case strong_type_n0(array:get(Idx, Types)) of
        other_neutral ->
            classify_bracket_content_1(Idx + 1, CloseIdx, Types, DirEmbed, DirOpposite);
        DirEmbed ->
            DirEmbed;
        Dir ->
            classify_bracket_content_1(Idx + 1, CloseIdx, Types, DirEmbed, Dir)
    end.


class_before_bracket({Start, _}, Types, SoS) ->
    class_before_bracket_1(Start - 1, Types, SoS).

class_before_bracket_1(Idx, Types, SoS) when Idx >= 0 ->
    case strong_type_n0(array:get(Idx, Types)) of
        other_neutral -> class_before_bracket_1(Idx - 1, Types, SoS);
        Type          -> Type
    end;

class_before_bracket_1(_, _, SoS) ->
    SoS.


set_brackets_to_type({OpenIdx, CloseIdx}, Type, Types, Indices, InitialTypes) ->
    T1 = array:set(OpenIdx, Type, array:set(CloseIdx, Type, Types)),
    T2 = set_brackets_nsm(OpenIdx + 1, CloseIdx, Type, T1, Indices, InitialTypes),
    set_brackets_nsm(CloseIdx + 1, array:size(Types), Type, T2, Indices, InitialTypes).


set_brackets_nsm(Idx, End, Type, Types, Indices, InitialTypes) when Idx < End ->
    ChIdx = array:get(Idx, Indices),
    case array:get(ChIdx, InitialTypes) of
        nonspacing_mark ->
            Types1 = array:set(Idx, Type, Types),
            set_brackets_nsm(Idx + 1, End, Type, Types1, Indices, InitialTypes);
        _ ->
            Types
    end;

set_brackets_nsm(_, _, _, Types, _, _) ->
    Types.


resolve_neutral_types(Ts0, Level, SoS, EoS) ->
    resolve_neutral_types(0, array:size(Ts0), Ts0, Level, SoS, EoS).

resolve_neutral_types(Idx, Idx, Types, _, _, _) ->
    Types;

resolve_neutral_types(Idx, Size, Types0, Level, SoS, EoS) ->
    case array:get(Idx, Types0) of
        Type when Type == white_space
                ; Type == other_neutral
                ; Type == paragraph_separator
                ; Type == segment_separator
                ; Type == right_to_left_isolate
                ; Type == left_to_right_isolate
                ; Type == first_strong_isolate
                ; Type == pop_directional_isolate ->
            RunStart = Idx,
            RunLimit = find_run_limit(RunStart, Size, Types0,
                                      [ paragraph_separator
                                      , segment_separator
                                      , white_space
                                      , other_neutral
                                      , right_to_left_isolate
                                      , left_to_right_isolate
                                      , first_strong_isolate
                                      , pop_directional_isolate
                                      ]),
            LeadingType = case RunStart of
                              0 -> SoS;
                              _ -> case array:get(RunStart - 1, Types0) of
                                       arabic_number -> right_to_left;
                                       european_number -> right_to_left;
                                       T1 -> T1
                                   end
                          end,
            TrailingType = case RunLimit of
                               Size -> EoS;
                               _ -> case array:get(RunLimit, Types0) of
                                       arabic_number -> right_to_left;
                                       european_number -> right_to_left;
                                       T2 -> T2
                                   end
                           end,

            ResolvedType = if
                               LeadingType == TrailingType -> LeadingType; % Rule N1
                               true -> type_for_level(Level) % Rule N2
                           end,

            Types1 = set_types(RunStart, RunLimit, ResolvedType, Types0),

            resolve_neutral_types(RunLimit, Size, Types1, Level, SoS, EoS);
        _ ->
            resolve_neutral_types(Idx + 1, Size, Types0, Level, SoS, EoS)
    end.


% resolving implicit embedding levels Rules I1, I2
resolve_implicit_levels(Ts, L, Indices, Levels) when (L band 1) == 0 ->
    array:foldl(fun (Idx, Type, Ls) ->
                        OrigIdx = array:get(Idx, Indices),
                        V = case Type of
                                left_to_right -> L;
                                right_to_left -> L + 1;
                                _             -> L + 2
                            end,
                        array:set(OrigIdx, V, Ls)
                end,
                Levels, Ts);

resolve_implicit_levels(Ts, L, Indices, Levels) ->
    array:foldl(fun (Idx, Type, Ls) ->
                        OrigIdx = array:get(Idx, Indices),
                        V = case Type of
                                right_to_left -> L;
                                _             -> L + 1
                            end,
                        array:set(OrigIdx, V, Ls)
                end,
                Levels, Ts).


assign_levels_to_characters_removed_by_X9(Types, Levels, EmbeddingLevel) ->
    Fun = fun (Idx, Type, {PrevLevel, L0}) ->
                  case is_removed_by_X9(Type) of
                      true  -> {PrevLevel, array:set(Idx, PrevLevel, L0)};
                      false -> {array:get(Idx, L0), L0}
                  end
          end,
    {_, L1} = array:foldl(Fun, {EmbeddingLevel, Levels}, Types),
    L1.


% Rule L1, clauses one, two and three
finalize_levels(Levels, Types, ParLevel) ->
    finalize_levels(0, array:size(Levels), Levels, Types, ParLevel).

finalize_levels(Idx, Idx, Levels, _, _) ->
    Levels;

finalize_levels(Idx, Size, Levels, Types, ParLevel) ->
    case array:get(Idx, Types) of
        T when T == paragraph_separator
             ; T == segment_separator ->
            L1 = array:set(Idx, ParLevel, Levels),
            L2 = finalize_levels_1(Idx - 1, L1, Types, ParLevel),
            finalize_levels(Idx + 1, Size, L2, Types, ParLevel);
        _ ->
            finalize_levels(Idx + 1, Size, Levels, Types, ParLevel)
    end.

% Rule L1, clause three
finalize_levels_1(Idx, Levels, Types, ParLevel) when Idx >= 0 ->
    Type = array:get(Idx, Types),
    case is_whitespace(Type) of
        true ->
            L1 = case is_removed_by_X9(Type) of
                     true  -> Levels;
                     false -> array:set(Idx, ParLevel, Levels)
                 end,
            finalize_levels_1(Idx - 1, L1, Types, ParLevel);
        false ->
            Levels
    end;

finalize_levels_1(_, Levels, _, _) ->
    Levels.


% Rule L1, clause four
get_levels(Levels, Types, ParLevel, LineBreaks) ->
    {L2, _} = lists:foldl(fun (LB, {LIn, Start}) ->
                                  LOut = get_levels_1(LB - 1, Start, LIn, Types, ParLevel),
                                  {LOut, LB}
                          end, {Levels, 0}, LineBreaks),
    L2.

get_levels_1(Idx, Start, Levels, Types, Level) when Idx >= Start ->
    Type = array:get(Idx, Types),
    case is_whitespace(Type) of
        true ->
            L1 = case is_removed_by_X9(Type) of
                     true  -> Levels;
                     false -> array:set(Idx, Level, Levels)
                 end,
            get_levels_1(Idx - 1, Start, L1, Types, Level);
        false ->
            Levels
    end;

get_levels_1(_, _, Levels, _, _) ->
    Levels.


hide_levels_for_characters_removed_by_X9(Types, Levels) ->
    array:foldr(fun (Idx, Type, Acc) ->
                    case is_removed_by_X9(Type) of
                        true  -> [hide | Acc];
                        false -> [array:get(Idx, Levels) | Acc]
                    end
                end, [], Types).


multiline_reordering(Levels, LineBreaks) ->
    multiline_reordering(Levels, LineBreaks, 0, []).


multiline_reordering(_, [], _, Acc) ->
    lists:flatten(lists:reverse(Acc));

multiline_reordering(Levels, [LineBreak | LineBreaks], Start, Acc0) ->
    Acc1 = compute_reordering(Levels, Start, LineBreak, Acc0),
    multiline_reordering(Levels, LineBreaks, LineBreak, Acc1).


compute_reordering(Levels, Start, Limit, Acc) ->
    {Highest, LowestOdd} = find_highest_lowest_odd_levels(Levels, Start, Limit),
    Idxs0 = array:from_list(lists:seq(Start, Limit - 1)),
    Idxs1 = compute_reordering_1(Highest, LowestOdd, Levels, Start, Limit, Idxs0),
    [array:to_list(Idxs1) | Acc].


compute_reordering_1(Level, LowestOdd, Levels, Start, Limit, Idxs0)
  when Level >= LowestOdd ->
    Idxs1 = compute_reordering_2(Level, Levels, Start, Limit, Start, Idxs0),
    compute_reordering_1(Level - 1, LowestOdd, Levels, Start, Limit, Idxs1);

compute_reordering_1(_, _, _, _, _, Idxs) ->
    Idxs.


compute_reordering_2(_, _, Idx, Idx, _, Idxs) ->
    Idxs;

compute_reordering_2(Level, Levels, Idx, Limit, Offset, Idxs0) ->
    case array:get(Idx, Levels) of
        L when L >= Level ->
            Start = Idx,
            End = range_at_or_above_level(Level, Levels, Idx + 1, Limit),
            Idxs1 = reverse_run(Start, End, Offset, Idxs0),
            compute_reordering_2(Level, Levels, End, Limit, Offset, Idxs1);
        _ ->
            compute_reordering_2(Level, Levels, Idx + 1, Limit, Offset, Idxs0)
    end.


find_highest_lowest_odd_levels(Levels, Start, Limit) ->
    find_highest_lowest_odd_levels(Levels, Start, Limit, 0, ?MAX_DEPTH + 2).

find_highest_lowest_odd_levels(_, Idx, Idx, Highest, LowestOdd) ->
    {Highest, LowestOdd};

find_highest_lowest_odd_levels(Levels, Idx, Limit, Highest0, LowestOdd0) ->
    Level = array:get(Idx, Levels),
    Highest1 = max(Level, Highest0),
    LowestOdd1 = if
                     (Level band 1) /= 0, Level < LowestOdd0 -> Level;
                     true -> LowestOdd0
                 end,
    find_highest_lowest_odd_levels(Levels, Idx+1, Limit, Highest1, LowestOdd1).


range_at_or_above_level(Level, Levels, Idx, Limit) when Idx < Limit ->
    case array:get(Idx, Levels) of
        L when L >= Level ->
            range_at_or_above_level(Level, Levels, Idx + 1, Limit);
        _ ->
            Idx
    end;

range_at_or_above_level(_, _, Idx, _) ->
    Idx.


reverse_run(Start, End, Offset, Idxs) ->
    reverse_run_1(Start - Offset, End - Offset -1, Idxs).

reverse_run_1(From, To, Idxs0) when From < To ->
    V1 = array:get(From, Idxs0),
    V2 = array:get(To, Idxs0),
    Idxs1 = array:set(From, V2, array:set(To, V1, Idxs0)),
    reverse_run_1(From + 1, To - 1, Idxs1);

reverse_run_1(_, _, Idxs) ->
    Idxs.


is_removed_by_X9(Type) ->
           Type == left_to_right_embedding
    orelse Type == right_to_left_embedding
    orelse Type == left_to_right_override
    orelse Type == right_to_left_override
    orelse Type == pop_directional_format
    orelse Type == boundary_neutral.


is_whitespace(Type) ->
           Type == left_to_right_embedding
    orelse Type == right_to_left_embedding
    orelse Type == left_to_right_override
    orelse Type == right_to_left_override
    orelse Type == pop_directional_format
    orelse Type == boundary_neutral
    orelse Type == white_space
    orelse Type == left_to_right_isolate
    orelse Type == right_to_left_isolate
    orelse Type == first_strong_isolate
    orelse Type == pop_directional_isolate.


type_for_level(Level) ->
    case Level band 1 of
        0 -> left_to_right;
        _ -> right_to_left
    end.


strong_type_n0(Type) when Type == european_number
                        ; Type == arabic_number
                        ; Type == arabic_letter
                        ; Type == right_to_left -> right_to_left;
strong_type_n0(left_to_right) -> left_to_right;
strong_type_n0(_)             -> other_neutral.


find_run_limit(Idx, Idx, _, _) ->
    Idx;

find_run_limit(Idx, Limit, Types, ValidTypes) ->
    case lists:member(array:get(Idx, Types), ValidTypes) of
        true  -> find_run_limit(Idx + 1, Limit, Types, ValidTypes);
        false -> Idx
    end.


set_types(Start, Limit, _, Types) when Start >= Limit ->
    Types;

set_types(Start, Limit, Type, Types) ->
    set_types(Start + 1, Limit, Type, array:set(Start, Type, Types)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bidirectional_class_test_() -> [
    ?_assertMatch(left_to_right, bidirectional_class($A))
   ,?_assertMatch(right_to_left, bidirectional_class(16#05be))
   ,?_assertMatch(arabic_letter, bidirectional_class(16#0608))

   ,?_assertMatch(european_number,     bidirectional_class($0))
   ,?_assertMatch(european_separator,  bidirectional_class($+))
   ,?_assertMatch(european_terminator, bidirectional_class($#))
   ,?_assertMatch(arabic_number,       bidirectional_class(16#0600))
   ,?_assertMatch(common_separator,    bidirectional_class($,))
   ,?_assertMatch(nonspacing_mark,     bidirectional_class(16#0300))
   ,?_assertMatch(boundary_neutral,    bidirectional_class($\b))

   ,?_assertMatch(paragraph_separator, bidirectional_class($\r))
   ,?_assertMatch(segment_separator,   bidirectional_class($\t))
   ,?_assertMatch(white_space,         bidirectional_class($\s))
   ,?_assertMatch(other_neutral,       bidirectional_class($!))

   ,?_assertMatch(left_to_right_embedding, bidirectional_class(16#202a))
   ,?_assertMatch(left_to_right_override,  bidirectional_class(16#202d))
   ,?_assertMatch(right_to_left_embedding, bidirectional_class(16#202b))
   ,?_assertMatch(right_to_left_override,  bidirectional_class(16#202e))
   ,?_assertMatch(pop_directional_format,  bidirectional_class(16#202c))
   ,?_assertMatch(left_to_right_isolate,   bidirectional_class(16#2066))
   ,?_assertMatch(right_to_left_isolate,   bidirectional_class(16#2067))
   ,?_assertMatch(first_strong_isolate,    bidirectional_class(16#2068))
   ,?_assertMatch(pop_directional_isolate, bidirectional_class(16#2069))
].

is_mirrored_test_() -> [
    ?_assert(is_mirrored($[))
   ,?_assert(is_mirrored($]))
   ,?_assert(not is_mirrored($*))
].

mirroring_glyph_test_() -> [
    ?_assertEqual($], mirroring_glyph($[))
   ,?_assertEqual($[, mirroring_glyph($]))
   ,?_assertEqual($«, mirroring_glyph($»))
   ,?_assertEqual($», mirroring_glyph($«))
   ,?_assertEqual(none, mirroring_glyph($*))
].


bracket_test_() -> [
    ?_assertEqual({open, [$]]},  bracket($[))
   ,?_assertEqual({close, [$[]}, bracket($]))
   ,?_assertEqual({open,  [16#232A, 16#3009]}, bracket(16#2329))
   ,?_assertEqual({close, [16#2329, 16#3008]}, bracket(16#232A))
   ,?_assertEqual({open,  [16#3009, 16#232A]}, bracket(16#3008))
   ,?_assertEqual({close, [16#3008, 16#2329]}, bracket(16#3009))
   ,?_assertEqual(none, bracket($»))
   ,?_assertEqual(none, bracket($«))
   ,?_assertEqual(none, bracket($*))
].

-define(ISOLATES(Ts),
        begin
          {MPDI, MII} = determine_matching_isolates(array:from_list(Ts)),
          {array:to_list(MPDI), array:to_list(MII)}
        end).

determine_matching_isolates_test_() -> [
     ?_assertMatch({[undefined, 3, undefined, undefined, undefined]
                   ,[undefined, undefined, undefined, 1, undefined]},
                   ?ISOLATES([ left_to_right
                             , left_to_right_isolate
                             , arabic_number
                             , pop_directional_isolate
                             , left_to_right ]))

    ,?_assertMatch({[undefined,6,4,undefined,undefined,undefined,undefined,undefined]
                   ,[undefined,undefined,undefined,undefined,2,undefined,1,undefined]},
                   ?ISOLATES([ arabic_number
                             , left_to_right_isolate
                             , right_to_left_isolate
                             , arabic_number
                             , pop_directional_isolate
                             , left_to_right
                             , pop_directional_isolate
                             , left_to_right ]))
].

-undef(ISOLATES).


-define(EMBEDDING_LEVEL(Ts),
        begin
          PTs = array:from_list(Ts),
          {MPDI, _} = determine_matching_isolates(PTs),
          determine_paragraph_embedding_level(PTs, MPDI)
        end).

determine_paragraph_embedding_level_test_() -> [
     ?_assertMatch(0, ?EMBEDDING_LEVEL([ left_to_right
                                       , right_to_left_isolate
                                       , arabic_number
                                       , pop_directional_isolate ]))

    ,?_assertMatch(1, ?EMBEDDING_LEVEL([ left_to_right_isolate
                                       , left_to_right
                                       , pop_directional_isolate
                                       , right_to_left ]))
].

-undef(EMBEDDING_LEVEL).

-define(LEVELS(Ts),
        begin
          PTs = array:from_list(Ts),
          {MPDI, _} = determine_matching_isolates(PTs),
          EmbeddingLevel = determine_paragraph_embedding_level(PTs, MPDI),
          {Ts1, Ls} = determine_explicit_embedding_levels(PTs, EmbeddingLevel, MPDI),
          {array:to_list(Ts1), array:to_list(Ls)}
        end).

determine_explicit_embedding_levels_test_() -> [
     ?_assertMatch({[ left_to_right
                    , left_to_right_isolate
                    , arabic_number
                    , pop_directional_isolate
                    , left_to_right ]
                   ,[0, 0, 2, 0, 0]},
                   ?LEVELS([ left_to_right
                           , left_to_right_isolate
                           , arabic_number
                           , pop_directional_isolate
                           , left_to_right ]))

    ,?_assertMatch({[ arabic_number
                    , left_to_right_isolate
                    , right_to_left_isolate
                    , arabic_number
                    , pop_directional_isolate
                    , left_to_right
                    , pop_directional_isolate
                    , left_to_right ]
                   ,[0, 0, 2, 3, 2, 2, 0, 0]},
                   ?LEVELS([arabic_number
                           ,left_to_right_isolate
                           ,right_to_left_isolate
                           ,arabic_number
                           ,pop_directional_isolate
                           ,left_to_right
                           ,pop_directional_isolate
                           ,left_to_right
                           ]))

    ,?_assertMatch({[ left_to_right
                    , right_to_left_embedding
                    , left_to_right
                    , left_to_right_override
                    , left_to_right
                    , pop_directional_format
                    , pop_directional_format
                    , left_to_right ]
                   ,[0, 1, 1, 2, 2, 2, 1, 0]},
                   ?LEVELS([ left_to_right
                           , right_to_left_embedding
                           , left_to_right
                           , left_to_right_override
                           , right_to_left
                           , pop_directional_format
                           , pop_directional_format
                           , left_to_right
                           ]))
].

-undef(LEVELS).


-define(LEVELS(Ls),array:from_list(Ls)).

multiline_reordering_test_() -> [
     ?_assertMatch([1,0,2,4,3,5,6],
                   multiline_reordering(?LEVELS([1,1,0,1,1,0,0]), [7]))

    ,?_assertMatch([12,13,11,10,9,7,8,6,5,4,3,2,1,0],
                   multiline_reordering(?LEVELS([1,1,1,1,1,1,1,2,2,1,1,1,2,2]), [14]))

    ,?_assertMatch([6,5,4,3,2,1,0,
                   12,13,11,10,9,7,8],
                   multiline_reordering(?LEVELS([1,1,1,1,1,1,1,2,2,1,1,1,2,2]), [7,14]))
].

-undef(LEVELS).

-endif.
