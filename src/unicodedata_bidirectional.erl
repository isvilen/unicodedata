-module(unicodedata_bidirectional).
-compile({parse_transform, unicodedata_ucd_transform}).
-export([ bidirectional_class/1
        , is_bidirectional_strong/1
        , is_bidirectional_weak/1
        , is_bidirectional_neutral/1
        , is_bidirectional_explicit/1
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


-spec is_bidirectional_strong(char()) -> boolean().
is_bidirectional_strong(CP) ->
    lists:member(bidirectional_class(CP), [ left_to_right
                                          , right_to_left
                                          , arabic_letter
                                          ]).


-spec is_bidirectional_weak(char()) -> boolean().
is_bidirectional_weak(CP) ->
    lists:member(bidirectional_class(CP), [ european_number
                                          , european_separator
                                          , european_terminator
                                          , arabic_number
                                          , common_separator
                                          , nonspacing_mark
                                          , boundary_neutral
                                          ]).


-spec is_bidirectional_neutral(char()) -> boolean().
is_bidirectional_neutral(CP) ->
    lists:member(bidirectional_class(CP), [ paragraph_separator
                                          , segment_separator
                                          , white_space
                                          , other_neutral
                                          ]).


-spec is_bidirectional_explicit(char()) -> boolean().
is_bidirectional_explicit(CP) ->
    lists:member(bidirectional_class(CP), [ left_to_right_embedding
                                          , left_to_right_override
                                          , right_to_left_embedding
                                          , right_to_left_override
                                          , pop_directional_format
                                          , left_to_right_isolate
                                          , right_to_left_isolate
                                          , first_strong_isolate
                                          , pop_directional_isolate
                                          ]).


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
-type index() :: non_neg_integer().

-record(paragraph,{ embedding_level :: non_neg_integer() | undefined
                  , codepoints :: array(char()) | undefined
                  , initial_types :: array(bidirectional_class())
                  , result_types :: array(bidirectional_class())
                  , matching_pdi :: array(undefined | index()) | undefined
                  , matching_isolate_initiator :: array(undefined | index()) | undefined
                  , levels :: array(non_neg_integer() | hide) | undefined
                  }).

-opaque paragraph() :: #paragraph{embedding_level :: non_neg_integer()}.


-spec paragraph(string()) -> paragraph().
paragraph(String) ->
    P0 = create_paragraph(String),
    P1 = determine_paragraph_embedding_level(P0),
    paragraph_run_uba(P1).


-spec paragraph(string(), EmbeddingLevel) -> paragraph()
      when  EmbeddingLevel :: non_neg_integer().
paragraph(String, EmbeddingLevel) ->
    P0 = create_paragraph(String),
    P1 = P0#paragraph{embedding_level = EmbeddingLevel},
    paragraph_run_uba(P1).


-spec embedding_level(paragraph()) -> non_neg_integer().
embedding_level(#paragraph{embedding_level=Level}) -> Level.


-spec embedding_levels(paragraph()) -> [non_neg_integer() | hide].
embedding_levels(#paragraph{codepoints=CPs}=Paragraph) ->
    embedding_levels(Paragraph, [array:size(CPs)]).


-spec embedding_levels(paragraph(), LineBreaks) -> [non_neg_integer() | hide]
      when LineBreaks :: [non_neg_integer()].
embedding_levels(Paragraph, LineBreaks) ->
    array:to_list(get_levels(Paragraph, LineBreaks)).


-spec embedding_level_kind(Level) -> left_to_right | right_to_left
      when Level :: non_neg_integer().
embedding_level_kind(Level) when is_integer(Level) ->
    type_for_level(Level).


-spec reorder_indices(paragraph()) -> [non_neg_integer()].
reorder_indices(#paragraph{codepoints=CPs}=Paragraph) ->
    reorder_indices(Paragraph, [array:size(CPs)]).


-spec reorder_indices(paragraph(), LineBreaks) -> [non_neg_integer()]
      when LineBreaks :: [non_neg_integer()].
reorder_indices(Paragraph, LineBreaks) ->
    Levels = get_levels(Paragraph, LineBreaks),
    EmbeddingLevel = Paragraph#paragraph.embedding_level,
    multiline_reordering(Levels, EmbeddingLevel, LineBreaks).


-spec reorder(paragraph()) -> string().
reorder(#paragraph{codepoints=CPs}=Paragraph) ->
    reorder(Paragraph, [array:size(CPs)]).


-spec reorder(paragraph(), LineBreaks) -> string()
      when LineBreaks :: [non_neg_integer()].
reorder(Paragraph, LineBreaks) ->
    Idxs = reorder_indices(Paragraph, LineBreaks),
    [array:get(Idx,Paragraph#paragraph.codepoints) || Idx <- Idxs].


create_paragraph(String) ->
    CPs = array:fix(array:from_list(String)),
    Types = array:fix(array:from_list([ucd_bidi_class(CP) || CP <- String])),
    determine_matching_isolates(#paragraph { codepoints = CPs
                                           , initial_types = Types
                                           , result_types = Types
                                           }).


find_embedding_level(_, Idx, Idx) ->
    0;

find_embedding_level(#paragraph{result_types=Types}=Paragraph, Idx, ToIdx) ->
    case array:get(Idx, Types) of
        left_to_right ->
            0;

        Type when Type == right_to_left
                ; Type == arabic_letter ->
            1;

        Type when Type == first_strong_isolate
                ; Type == left_to_right_isolate
                ; Type == right_to_left_isolate ->
            MatchingPDI = Paragraph#paragraph.matching_pdi,
            find_embedding_level(Paragraph, array:get(Idx, MatchingPDI), ToIdx);

        _ ->
            find_embedding_level(Paragraph, Idx + 1, ToIdx)
    end.


determine_paragraph_embedding_level(#paragraph{result_types=Types}=Paragraph) ->
    EmbeddingLevel = find_embedding_level(Paragraph, 0, array:size(Types)),
    Paragraph#paragraph{embedding_level = EmbeddingLevel}.


determine_matching_isolates(#paragraph{result_types=Types}=Paragraph) ->
    Size = array:size(Types),
    MPDI = array:new(Size, [{default, undefined}, fixed]),
    MII = array:new(Size, [{default, undefined}, fixed]),
    determine_matching_isolates(Paragraph, 0, array:size(Types), MPDI, MII).

determine_matching_isolates(Paragraph, Idx, Idx, MPDI, MII) ->
    Paragraph#paragraph{ matching_pdi = MPDI
                       , matching_isolate_initiator = MII
                       };

determine_matching_isolates(Paragraph, Idx, ToIdx, MPDI, MII) ->
    case array:get(Idx, Paragraph#paragraph.result_types) of
        Type when Type == first_strong_isolate
                ; Type == left_to_right_isolate
                ; Type == right_to_left_isolate ->
            determine_matching_isolates_1(Paragraph, Idx, Idx + 1, ToIdx, 1, MPDI, MII);
        _ ->
            determine_matching_isolates(Paragraph, Idx + 1, ToIdx, MPDI, MII)
    end.


determine_matching_isolates_1(Paragraph, StartIdx, ToIdx, ToIdx, _, MPDI, MII) ->
    MPDI1 = array:set(StartIdx, ToIdx, MPDI),
    determine_matching_isolates(Paragraph, StartIdx + 1, ToIdx, MPDI1, MII);

determine_matching_isolates_1(Paragraph, StartIdx, Idx, ToIdx, Depth, MPDI, MII) ->
    case array:get(Idx, Paragraph#paragraph.result_types) of
        Type when Type == first_strong_isolate
                ; Type == left_to_right_isolate
                ; Type == right_to_left_isolate ->
            determine_matching_isolates_1(Paragraph, StartIdx, Idx + 1, ToIdx,
                                          Depth + 1, MPDI, MII);

        pop_directional_isolate when Depth > 1 ->
            determine_matching_isolates_1(Paragraph, StartIdx, Idx + 1, ToIdx,
                                          Depth - 1, MPDI, MII);

        pop_directional_isolate ->
            MPDI1 = array:set(StartIdx, Idx, MPDI),
            MII1 = array:set(Idx, StartIdx, MII),
            determine_matching_isolates(Paragraph, StartIdx + 1, ToIdx,
                                        MPDI1, MII1);

        _ ->
            determine_matching_isolates_1(Paragraph, StartIdx, Idx + 1, ToIdx,
                                          Depth, MPDI, MII)
    end.


paragraph_run_uba(P1) ->
    P2 = init_embedding_levels(P1),
    P3 = determine_explicit_embedding_levels(P2),
    Sequences = determine_isolating_run_sequences(P3),
    P4 = lists:foldl(fun process_isolating_run_sequence/2, P3, Sequences),
    hide_characters_removed_by_X9(P4).


init_embedding_levels(#paragraph{embedding_level=Level, result_types=Types}=P0) ->
    Levels = array:new(array:size(Types), [{default, Level}, fixed]),
    P0#paragraph{levels=Levels}.


% directional status state
-record(uba_ds,{ paragraph
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
                        ,S#uba_ds.overflow_isolate_count == 0
                        ,S#uba_ds.overflow_embedding_count == 0).


determine_explicit_embedding_levels(Paragraph) ->
    Types = Paragraph#paragraph.result_types,
    Level = Paragraph#paragraph.embedding_level,
    State = uba_ds_push(Level, other_neutral, false, #uba_ds{paragraph=Paragraph}),
    determine_explicit_embedding_levels_1(0, array:size(Types), State).


determine_explicit_embedding_levels_1(Idx, Idx, #uba_ds{paragraph=Paragraph}) ->
    Paragraph;

determine_explicit_embedding_levels_1(Idx, LastIdx, S0) ->
    S1 = determine_explicit_embedding_levels_2(Idx, uba_ds_type(Idx, S0), S0),
    determine_explicit_embedding_levels_1(Idx + 1, LastIdx, S1).


determine_explicit_embedding_levels_2(Idx, Type, S0)
  when Type == right_to_left_isolate
     ; Type == left_to_right_isolate
     ; Type == first_strong_isolate ->

    S1 = case uba_ds_last_override_status(S0) of
             other_neutral      -> S0;
             LastOverrideStatus -> uba_ds_set_type(Idx, LastOverrideStatus, S0)
         end,

    S2 = uba_ds_set_last_embedding_level(Idx, S1),

    case uba_ds_next_embedding_level(Idx, Type, S0) of
        Level when ?CHECK_LEVEL(Level,S0) ->
            S3 = uba_ds_push(Level, Type, true, S2),
            uba_ds_add_valid_isolate_count(S3);
        _ ->
            uba_ds_add_overflow_isolate_count(S2)
    end;

determine_explicit_embedding_levels_2(Idx, Type, S0)
  when Type == right_to_left_embedding
     ; Type == left_to_right_embedding
     ; Type == right_to_left_override
     ; Type == left_to_right_override ->

    case uba_ds_next_embedding_level(Idx, Type, S0) of
        Level when ?CHECK_LEVEL(Level,S0) ->
            S1 = uba_ds_push(Level, Type, false, S0),
            uba_ds_set_embedding_level(Idx, Level, S1);
        _ ->
            case uba_ds_set_last_embedding_level(Idx, S0) of
                S1 when S1#uba_ds.overflow_isolate_count == 0 ->
                    uba_ds_add_overflow_embedding_count(S1);
                S1 -> S1
            end
    end;

determine_explicit_embedding_levels_2(Idx, pop_directional_isolate, S0) ->
    S1 = if
             S0#uba_ds.overflow_isolate_count > 0 ->
                 uba_ds_sub_overflow_isolate_count(S0);

             S0#uba_ds.valid_isolate_count == 0 ->
                 S0;

             true ->
                 uba_ds_pop_directional_isolate(S0#uba_ds{overflow_embedding_count=0})
         end,
    uba_ds_set_last_embedding_level(Idx, S1);

determine_explicit_embedding_levels_2(Idx, pop_directional_format, S0) ->
    S1 = uba_ds_set_last_embedding_level(Idx, S0),
    if
        S1#uba_ds.overflow_isolate_count > 0 ->
            S1;

        S1#uba_ds.overflow_embedding_count > 0 ->
            uba_ds_sub_overflow_embedding_count(S1);

        true ->
            case uba_ds_last_isolate_status(S1) of
                false when S1#uba_ds.counter >= 2 -> uba_ds_pop(S1);
                _                                 -> S1
            end
    end;

determine_explicit_embedding_levels_2(Idx, paragraph_separator, #uba_ds{paragraph=P0}) ->
    EmbeddingLevel = P0#paragraph.embedding_level,
    P1 = P0#paragraph{levels=array:set(Idx, EmbeddingLevel, P0#paragraph.levels)},
    #uba_ds{paragraph = P1};

determine_explicit_embedding_levels_2(Idx, _, S0) ->
    S1 = uba_ds_set_last_embedding_level(Idx, S0),
    case uba_ds_last_override_status(S1) of
        other_neutral      -> S1;
        LastOverrideStatus -> uba_ds_set_type(Idx, LastOverrideStatus, S1)
    end.


uba_ds_push(Level, OverrideType, IsolateStatus,
            #uba_ds{ counter=Counter
                   , embedding_level = Ls
                   , override_status = OSs
                   , isolate_status = ISs
                   }=S0) ->
    OverrideStatus = case OverrideType of
                         left_to_right_override -> left_to_right;
                         right_to_left_override -> right_to_left;
                         _                      -> other_neutral
                     end,
    S0#uba_ds{ counter = Counter + 1
             , embedding_level = [Level | Ls]
             , override_status = [OverrideStatus | OSs]
             , isolate_status = [IsolateStatus | ISs]
             }.


uba_ds_pop(#uba_ds{ counter=Counter
                  , embedding_level = [_ | Ls]
                  , override_status = [_ | OSs]
                  , isolate_status = [_ | ISs]
                  }=S0) ->

    S0#uba_ds{ counter = Counter - 1
             , embedding_level = Ls
             , override_status = OSs
             , isolate_status = ISs
             }.

uba_ds_set_last_embedding_level(Idx, #uba_ds{paragraph=P0,embedding_level=Ls}=S0) ->
    Levels = P0#paragraph.levels,
    P1 = P0#paragraph{levels=array:set(Idx, hd(Ls), Levels)},
    S0#uba_ds{paragraph=P1}.

uba_ds_last_embedding_level(#uba_ds{embedding_level=[L|_]}) -> L.


uba_ds_set_embedding_level(Idx, Level, #uba_ds{paragraph=P0}=S0) ->
    Levels = P0#paragraph.levels,
    P1 = P0#paragraph{levels=array:set(Idx, Level, Levels)},
    S0#uba_ds{paragraph=P1}.


uba_ds_pop_directional_isolate(#uba_ds{isolate_status=[false|_]}=S0) ->
    uba_ds_pop_directional_isolate(uba_ds_pop(S0));

uba_ds_pop_directional_isolate(S0) ->
    S1 = uba_ds_pop(S0),
    S1#uba_ds{valid_isolate_count = S1#uba_ds.valid_isolate_count - 1}.


uba_ds_next_embedding_level(Idx, Type, #uba_ds{paragraph=P}=S) ->
    IsRTL = case Type of
                first_strong_isolate ->
                    find_embedding_level(P
                                        ,Idx + 1
                                        ,array:get(Idx, P#paragraph.matching_pdi)
                                        ) == 1;
                right_to_left_embedding -> true;
                right_to_left_override  -> true;
                right_to_left_isolate   -> true;
                _                       -> false
            end,
    case IsRTL of
        true -> % least greater odd
            (uba_ds_last_embedding_level(S) + 1) bor 1;

        false -> % least greater even
            (uba_ds_last_embedding_level(S) + 2) band (bnot 1)
    end.


uba_ds_add_valid_isolate_count(#uba_ds{valid_isolate_count=V}=S) ->
    S#uba_ds{valid_isolate_count = V + 1}.


uba_ds_add_overflow_isolate_count(#uba_ds{overflow_isolate_count=V}=S) ->
    S#uba_ds{overflow_isolate_count = V + 1}.

uba_ds_sub_overflow_isolate_count(#uba_ds{overflow_isolate_count=V}=S) ->
    S#uba_ds{overflow_isolate_count = V - 1}.


uba_ds_add_overflow_embedding_count(#uba_ds{overflow_embedding_count=V}=S) ->
    S#uba_ds{overflow_embedding_count = V + 1}.

uba_ds_sub_overflow_embedding_count(#uba_ds{overflow_embedding_count=V}=S) ->
    S#uba_ds{overflow_embedding_count = V - 1}.


uba_ds_last_isolate_status(#uba_ds{isolate_status=[V|_]}) -> V.


uba_ds_last_override_status(#uba_ds{override_status=[V|_]}) -> V.


uba_ds_type(Idx, #uba_ds{paragraph=P}) ->
    array:get(Idx, P#paragraph.result_types).

uba_ds_set_type(Idx, Type, #uba_ds{paragraph=P0}=S) ->
    Types = P0#paragraph.result_types,
    P1 = P0#paragraph{result_types=array:set(Idx, Type, Types)},
    S#uba_ds{paragraph=P1}.


% isolating run sequence
-record(uba_irs,{ indices :: array(index())
                , types :: array(bidirectional_class())
                , resolved_levels :: array(non_neg_integer())
                , level :: non_neg_integer()
                , sos :: bidirectional_class()
                , eos :: bidirectional_class()
                }).

%% definition BD13
determine_isolating_run_sequences(Paragraph) ->
    LevelRuns = determine_level_runs(Paragraph),
    RunForCh = run_for_character(LevelRuns),
    Seqs = array:foldl(
             fun (_, Run, Acc) ->
                     determine_isolating_run_sequence(Run, LevelRuns, RunForCh, Paragraph, Acc)
             end, [], LevelRuns),
    lists:reverse(Seqs).

determine_isolating_run_sequence([FirstCh|_]=Run, LevelRuns, RunForCh, Paragraph, Acc) ->
    case array:get(FirstCh, Paragraph#paragraph.initial_types) of
        T when T /= pop_directional_isolate ->
            [determine_isolating_run_sequence_1(LevelRuns, RunForCh, Paragraph, Run) | Acc];
        _ ->
            case array:get(FirstCh, Paragraph#paragraph.matching_isolate_initiator) of
                undefined ->
                    [determine_isolating_run_sequence_1(LevelRuns, RunForCh, Paragraph, Run) | Acc];
                _ ->
                    Acc
            end
    end.

determine_isolating_run_sequence_1(LevelRuns, RunForCh, Paragraph, Run) ->
    LastCh = lists:last(Run),
    case array:get(LastCh, Paragraph#paragraph.initial_types) of
        T when T == left_to_right_isolate
             ; T == right_to_left_isolate
             ; T == first_strong_isolate ->
            MatchingPDI = array:get(LastCh, Paragraph#paragraph.matching_pdi),
            TextLength = array:size(Paragraph#paragraph.initial_types),
            if
                MatchingPDI /= TextLength ->
                    NextRunIdx = maps:get(MatchingPDI, RunForCh),
                    NextRun = array:get(NextRunIdx, LevelRuns),
                    determine_isolating_run_sequence_1(LevelRuns, RunForCh, Paragraph, Run ++ NextRun);
                true ->
                    create_isolating_run_sequence(Run, Paragraph)
            end;
        _ ->
            create_isolating_run_sequence(Run, Paragraph)
    end.


create_isolating_run_sequence(Run, Paragraph) ->
    Indices = array:fix(array:from_list(Run)),

    Types = Paragraph#paragraph.result_types,
    SeqTypes = array:fix(array:from_list([array:get(Idx, Types) || Idx <- Run])),

    Level = array:get(hd(Run), Paragraph#paragraph.levels),
    SeqLevels = array:new(array:size(Indices), [{default, Level}, fixed]),

    PrevLevel = isolating_run_sequence_prev_level(Run, Paragraph),
    SuccLevel = isolating_run_sequence_succ_level(Run, Paragraph),

    #uba_irs{ indices = Indices
            , types = SeqTypes
            , resolved_levels = SeqLevels
            , level = Level
            , sos = type_for_level(max(PrevLevel, Level))
            , eos = type_for_level(max(SuccLevel, Level))
            }.


isolating_run_sequence_prev_level([Idx | _], Paragraph) ->
    isolating_run_sequence_prev_level_1(Idx - 1, Paragraph).

isolating_run_sequence_prev_level_1(Idx, Paragraph) when Idx >= 0 ->
    case is_removed_by_X9(array:get(Idx, Paragraph#paragraph.initial_types)) of
        true  -> isolating_run_sequence_prev_level_1(Idx - 1, Paragraph);
        false -> array:get(Idx, Paragraph#paragraph.levels)
    end;

isolating_run_sequence_prev_level_1(_, Paragraph) ->
    Paragraph#paragraph.embedding_level.


isolating_run_sequence_succ_level(Run, Paragraph) ->
    LastIdx = lists:last(Run),
    case array:get(LastIdx, Paragraph#paragraph.result_types) of
        Type when Type == left_to_right_isolate
                ; Type == right_to_left_isolate
                ; Type == first_strong_isolate ->
            Paragraph#paragraph.embedding_level;
        _ ->
            isolating_run_sequence_succ_level_1(LastIdx + 1, Paragraph)
    end.

isolating_run_sequence_succ_level_1(Idx, Paragraph) ->
    Size = array:size(Paragraph#paragraph.initial_types),
    if
        Idx >= Size ->
            Paragraph#paragraph.embedding_level;
        true ->
            case is_removed_by_X9(array:get(Idx, Paragraph#paragraph.initial_types)) of
                true  ->
                    isolating_run_sequence_succ_level_1(Idx+1, Paragraph);
                false ->
                    array:get(Idx, Paragraph#paragraph.levels)
            end
    end.


determine_level_runs(Paragraph) ->
    Types = Paragraph#paragraph.initial_types,
    Levels = Paragraph#paragraph.levels,
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


process_isolating_run_sequence(S0, P) ->
    % Rules W1-W7
    S1 = resolve_weak_types(S0),

    % Rule N0
    S2 = resolve_paired_brackets(S1, P#paragraph.codepoints, P#paragraph.initial_types),

    % Rules N1-N3
    S3 = resolve_neutral_types(S2),

    % Rules I1, I2
    S4 = resolve_implicit_levels(S3),

    apply_levels_and_types(S4, P).


resolve_weak_types(S0) ->
    S1 = resolve_weak_types_w1(S0),
    S2 = resolve_weak_types_w2(S1),
    S3 = resolve_weak_types_w3(S2),
    S4 = resolve_weak_types_w4(S3),
    S5 = resolve_weak_types_w5(S4),
    S6 = resolve_weak_types_w6(S5),
    resolve_weak_types_w7(S6).


% Rule W1: Changes all NSMs
resolve_weak_types_w1(#uba_irs{types=Ts0,sos=SoS}=S) ->
    Ts1 = resolve_weak_types_w1(0, array:size(Ts0), Ts0, SoS),
    S#uba_irs{types=Ts1}.

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
resolve_weak_types_w2(#uba_irs{types=Ts0}=S) ->
    Ts1 = resolve_weak_types_w2(0, array:size(Ts0), Ts0),
    S#uba_irs{types=Ts1}.

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
resolve_weak_types_w3(#uba_irs{types=Ts0}=S) ->
    Ts1 = resolve_weak_types_w3(0, array:size(Ts0), Ts0),
    S#uba_irs{types=Ts1}.

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
resolve_weak_types_w4(#uba_irs{types=Ts0}=S) ->
    Ts1 = resolve_weak_types_w4(1, array:size(Ts0) - 1, Ts0),
    S#uba_irs{types=Ts1}.


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
resolve_weak_types_w5(#uba_irs{types=Ts0,sos=SoS,eos=EoS}=S) ->
    Ts1 = resolve_weak_types_w5(0, array:size(Ts0), Ts0, SoS, EoS),
    S#uba_irs{types=Ts1}.

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
resolve_weak_types_w6(#uba_irs{types=Ts0}=S) ->
    Ts1 = resolve_weak_types_w6(0, array:size(Ts0), Ts0),
    S#uba_irs{types=Ts1}.

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
resolve_weak_types_w7(#uba_irs{types=Ts0, sos=SoS}=S) ->
    Ts1 = resolve_weak_types_w7(0, array:size(Ts0), Ts0, SoS),
    S#uba_irs{types=Ts1}.

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

resolve_paired_brackets(#uba_irs{ indices = Indices
                                , types = Types0
                                , level = Level
                                , sos = SoS
                                } = S,
                        CPs, InitialTypes) ->
    Brackets = locate_brackets(Indices, Types0, CPs),
    Types1 = resolve_brackets(Brackets, Types0, SoS, type_for_level(Level),
                              Indices, InitialTypes),
    S#uba_irs{types = Types1}.


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
            case class_before_bracket(Bracket, Types, SoS) of
                other_neutral ->
                    set_brackets_to_type(Bracket, DirEmbed, Types, Indices, InitialTypes);
                V ->
                    set_brackets_to_type(Bracket, V, Types, Indices, InitialTypes)
            end
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


resolve_neutral_types(#uba_irs{types=Ts}=S) ->
    resolve_neutral_types(0, array:size(Ts), S).

resolve_neutral_types(Idx, Idx, S) ->
    S;

resolve_neutral_types(Idx, Size, S) ->
    case array:get(Idx, S#uba_irs.types) of
        Type when Type == white_space
                ; Type == other_neutral
                ; Type == paragraph_separator
                ; Type == segment_separator
                ; Type == right_to_left_isolate
                ; Type == left_to_right_isolate
                ; Type == first_strong_isolate
                ; Type == pop_directional_isolate ->
            Types0 = S#uba_irs.types,
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
                              0 -> S#uba_irs.sos;
                              _ -> case array:get(RunStart - 1, Types0) of
                                       arabic_number -> right_to_left;
                                       european_number -> right_to_left;
                                       T1 -> T1
                                   end
                          end,
            TrailingType = case RunLimit of
                               Size -> S#uba_irs.eos;
                               _ -> case array:get(RunLimit, Types0) of
                                       arabic_number -> right_to_left;
                                       european_number -> right_to_left;
                                       T2 -> T2
                                   end
                           end,

            ResolvedType = if
                               LeadingType == TrailingType -> LeadingType; % Rule N1
                               true -> type_for_level(S#uba_irs.level) % Rule N2
                           end,

            Types1 = set_types(RunStart, RunLimit, ResolvedType, Types0),

            resolve_neutral_types(RunLimit, Size, S#uba_irs{types = Types1});
        _ ->
            resolve_neutral_types(Idx + 1, Size, S)
    end.


% resolving implicit embedding levels Rules I1, I2
resolve_implicit_levels(#uba_irs{level=L,types=Ts}=Seq) when (L band 1) == 0 ->
    Levels = array:foldl(fun (_,   left_to_right, Ls) -> Ls;
                             (Idx, right_to_left, Ls) -> array:set(Idx, array:get(Idx, Ls) + 1, Ls);
                             (Idx, _, Ls)             -> array:set(Idx, array:get(Idx, Ls) + 2, Ls)
                         end,
                         Seq#uba_irs.resolved_levels, Ts),
     Seq#uba_irs{resolved_levels=Levels};

resolve_implicit_levels(#uba_irs{types=Ts}=Seq) ->
    Levels = array:foldl(fun (_, right_to_left, Ls) -> Ls;
                             (Idx, _, Ls)           -> array:set(Idx, array:get(Idx, Ls) + 1, Ls)
                         end,
                         Seq#uba_irs.resolved_levels, Ts),
     Seq#uba_irs{resolved_levels=Levels}.


apply_levels_and_types(Seq, Paragraph) ->
    Types = Seq#uba_irs.types,
    Levels = Seq#uba_irs.resolved_levels,
    array:foldl(fun (Idx, OrigIdx, P) ->
                        Type = array:get(Idx, Types),
                        Level = array:get(Idx, Levels),
                        PTypes = P#paragraph.result_types,
                        PLevels = P#paragraph.levels,
                        P#paragraph{ result_types = array:set(OrigIdx, Type, PTypes)
                                   , levels = array:set(OrigIdx, Level, PLevels)
                                   }
                end, Paragraph, Seq#uba_irs.indices).


hide_characters_removed_by_X9(Paragraph) ->
    array:foldl(fun (Idx, Type, P) when Type == left_to_right_embedding
                                      ; Type == right_to_left_embedding
                                      ; Type == left_to_right_override
                                      ; Type == right_to_left_override
                                      ; Type == pop_directional_format
                                      ; Type == boundary_neutral ->
                        Types = P#paragraph.result_types,
                        Levels = P#paragraph.levels,
                        P#paragraph{ result_types = array:set(Idx, Type, Types)
                                   , levels = array:set(Idx, hide, Levels)
                                   };
                    (_, _, P) ->
                        P
                end, Paragraph, Paragraph#paragraph.initial_types).


get_levels(Paragraph, LineBreaks) ->
    L0 = Paragraph#paragraph.levels,
    L1 = get_levels_1(0, array:size(L0), L0, Paragraph),
    get_levels_2(L1, Paragraph, LineBreaks).


% Rule L1, clauses one, two and three
get_levels_1(Idx, Idx, Levels, _) ->
    Levels;

get_levels_1(Idx, Size, Levels, Paragraph) ->
    case array:get(Idx, Paragraph#paragraph.initial_types) of
        T when T == paragraph_separator
             ; T == segment_separator ->
            L1 = array:set(Idx, Paragraph#paragraph.embedding_level, Levels),
            L2 = get_levels_1_1(Idx - 1, L1, Paragraph),
            get_levels_1(Idx + 1, Size, L2, Paragraph);
        _ ->
            get_levels_1(Idx + 1, Size, Levels, Paragraph)
    end.

% Rule L1, clause three
get_levels_1_1(Idx, Levels, Paragraph) when Idx >= 0 ->
    case is_whitespace(array:get(Idx, Paragraph#paragraph.initial_types)) of
        true ->
            L1 = case array:get(Idx, Levels) of
                     hide -> Levels;
                     _    -> array:set(Idx, Paragraph#paragraph.embedding_level, Levels)
                 end,
            get_levels_1_1(Idx - 1, L1, Paragraph);
        false ->
            Levels
    end;

get_levels_1_1(_, Levels, _) ->
    Levels.


% Rule L1, clause four
get_levels_2(Levels, Paragraph, LineBreaks) ->
    Types = Paragraph#paragraph.initial_types,
    ParLevel = Paragraph#paragraph.embedding_level,
    {L2, _} = lists:foldl(fun (LB, {LIn, Start}) ->
                                  LOut = get_levels_2_1(LB - 1, Start, LIn, Types, ParLevel),
                                  {LOut, LB}
                          end, {Levels, 0}, LineBreaks),
    L2.

get_levels_2_1(Idx, Start, Levels, Types, Level) when Idx >= Start ->
    case is_whitespace(array:get(Idx, Types)) of
        true ->
            L1 = case array:get(Idx, Levels) of
                     hide -> Levels;
                     _    -> array:set(Idx, Level, Levels)
                 end,
            get_levels_2_1(Idx - 1, Start, L1, Types, Level);
        false ->
            Levels
    end;

get_levels_2_1(_, _, Levels, _, _) ->
    Levels.


multiline_reordering(Levels, EmbeddingLevel, LineBreaks) ->
    Levels1 = assign_levels_to_characters_removed_by_X9(Levels, EmbeddingLevel),
    Idxs = multiline_reordering(Levels1, LineBreaks, 0, []),
    [Idx || Idx <- Idxs, array:get(Idx, Levels) /= hide].


multiline_reordering(_, [], _, Acc) ->
    lists:reverse(Acc);

multiline_reordering(Levels, [LineBreak | LineBreaks], Start, Acc0) ->
    Acc1 = compute_reordering(Levels, Start, LineBreak, Acc0),
    multiline_reordering(Levels, LineBreaks, LineBreak, Acc1).


compute_reordering(Levels, Start, Limit, Acc) ->
    {Highest, LowestOdd} = find_highest_lowest_odd_levels(Levels, Start, Limit),
    Idxs0 = array:from_list(lists:seq(Start, Limit - 1)),
    Idxs1 = compute_reordering_1(Highest, LowestOdd, Levels, Start, Limit, Idxs0),
    array:foldl(fun (_, V, Acc0) -> [V | Acc0] end, Acc, Idxs1).


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


assign_levels_to_characters_removed_by_X9(Levels, EmbeddingLevel) ->
    {_, L1} = array:foldl(fun (Idx, hide, {PrevLevel, L0}) ->
                                  {PrevLevel, array:set(Idx, PrevLevel, L0)};
                              (_, Level, {_, L0}) ->
                                  {Level, L0}
                          end, {EmbeddingLevel, Levels}, Levels),
    L1.


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

   ,?_assert(is_bidirectional_strong($A))
   ,?_assert(is_bidirectional_strong(16#5be))
   ,?_assert(is_bidirectional_strong(16#608))
   ,?_assert(not is_bidirectional_strong($0))

   ,?_assert(is_bidirectional_weak($0))
   ,?_assert(is_bidirectional_weak($+))
   ,?_assert(is_bidirectional_weak($#))
   ,?_assert(is_bidirectional_weak(16#0600))
   ,?_assert(is_bidirectional_weak($,))
   ,?_assert(is_bidirectional_weak(16#0300))
   ,?_assert(is_bidirectional_weak($\b))
   ,?_assert(not is_bidirectional_weak($a))

   ,?_assert(is_bidirectional_neutral($\r))
   ,?_assert(is_bidirectional_neutral($\t))
   ,?_assert(is_bidirectional_neutral($\s))
   ,?_assert(is_bidirectional_neutral($!))
   ,?_assert(not is_bidirectional_neutral($,))

   ,?_assert(is_bidirectional_explicit(16#202a))
   ,?_assert(is_bidirectional_explicit(16#202d))
   ,?_assert(is_bidirectional_explicit(16#202b))
   ,?_assert(is_bidirectional_explicit(16#202e))
   ,?_assert(is_bidirectional_explicit(16#202c))
   ,?_assert(is_bidirectional_explicit(16#2066))
   ,?_assert(is_bidirectional_explicit(16#2067))
   ,?_assert(is_bidirectional_explicit(16#2068))
   ,?_assert(is_bidirectional_explicit(16#2069))
   ,?_assert(not is_bidirectional_explicit($A))
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

-define(PARAGRAPH(Ts),
        begin
          PTs = array:from_list(Ts),
          P0 = #paragraph{initial_types=PTs, result_types=PTs},
          P1 = determine_matching_isolates(P0),
          {array:to_list(P1#paragraph.matching_pdi)
          ,array:to_list(P1#paragraph.matching_isolate_initiator)}
        end).

determine_matching_isolates_test_() -> [
     ?_assertMatch({[undefined, 3, undefined, undefined, undefined]
                   ,[undefined, undefined, undefined, 1, undefined]},
                   ?PARAGRAPH([ left_to_right
                              , left_to_right_isolate
                              , arabic_number
                              , pop_directional_isolate
                              , left_to_right ]))

    ,?_assertMatch({[undefined,6,4,undefined,undefined,undefined,undefined,undefined]
                   ,[undefined,undefined,undefined,undefined,2,undefined,1,undefined]},
                   ?PARAGRAPH([ arabic_number
                              , left_to_right_isolate
                              , right_to_left_isolate
                              , arabic_number
                              , pop_directional_isolate
                              , left_to_right
                              , pop_directional_isolate
                              , left_to_right ]))
].

-undef(PARAGRAPH).


-define(PARAGRAPH(Ts),
        begin
          PTs = array:from_list(Ts),
          P0 = #paragraph{initial_types=PTs, result_types=PTs},
          P1 = determine_matching_isolates(P0),
          P2 = determine_paragraph_embedding_level(P1),
          P2#paragraph.embedding_level
        end).

determine_paragraph_embedding_level_test_() -> [
     ?_assertMatch(0, ?PARAGRAPH([ left_to_right
                                 , right_to_left_isolate
                                 , arabic_number
                                 , pop_directional_isolate ]))

    ,?_assertMatch(1, ?PARAGRAPH([ left_to_right_isolate
                                 , left_to_right
                                 , pop_directional_isolate
                                 , right_to_left ]))
].

-undef(PARAGRAPH).

-define(PARAGRAPH(Ts),
        begin
          PTs = array:from_list(Ts),
          P0 = #paragraph{initial_types=PTs, result_types=PTs},
          P1 = determine_matching_isolates(P0),
          P2 = determine_paragraph_embedding_level(P1),
          P3 = init_embedding_levels(P2),
          P4 = determine_explicit_embedding_levels(P3),
          {array:to_list(P4#paragraph.result_types),array:to_list(P4#paragraph.levels)}
        end).

determine_explicit_embedding_levels_test_() -> [
     ?_assertMatch({[ left_to_right
                    , left_to_right_isolate
                    , arabic_number
                    , pop_directional_isolate
                    , left_to_right ]
                   ,[0, 0, 2, 0, 0]},
                   ?PARAGRAPH([ left_to_right
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
                   ?PARAGRAPH([arabic_number
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
                   ?PARAGRAPH([ left_to_right
                              , right_to_left_embedding
                              , left_to_right
                              , left_to_right_override
                              , right_to_left
                              , pop_directional_format
                              , pop_directional_format
                              , left_to_right
                              ]))

].

-undef(PARAGRAPH).


multiline_reordering_test_() -> [
     ?_assertMatch([1,0,2,4,3,5,6],
                   multiline_reordering(lv([1,1,0,1,1,0,0]), 0, [7]))

    ,?_assertMatch([12,13,11,10,9,7,8,6,5,4,3,2,1,0],
                   multiline_reordering(lv([1,1,1,1,1,1,1,2,2,1,1,1,2,2]), 1, [14]))

    ,?_assertMatch([6,5,4,3,2,1,0,
                   12,13,11,10,9,7,8],
                   multiline_reordering(lv([1,1,1,1,1,1,1,2,2,1,1,1,2,2]), 1, [7,14]))

    ,?_assertMatch([11,9,7,6,5,3,1],
                   multiline_reordering(lv([x,1,x,2,x,1,2,1,x,2,x,1,x]), 0, [13]))

    ,?_assertMatch([6,5,4,2,9,10],
                   multiline_reordering(lv([x,x,3,x,3,3,3,x,x,2,2,x]), 0, [12]))

    ,?_assertMatch([8,9,10,11,5,4,2],
                   multiline_reordering(lv([x,x,4,x,3,3,x,x,4,4,4,4]), 1, [12]))
].

lv(L) -> array:from_list([case V of x -> hide; _ -> V end || V <- L]).

-endif.
