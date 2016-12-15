-module(unicodedata_segmentation).
-compile({parse_transform, ucd_transform}).
-export([ grapheme_breaks/3
        , word_breaks/3
        , sentence_breaks/3
        , line_breaks/3
        ]).

-spec grapheme_breaks(Fun, Acc0, Text) -> Acc1
      when Fun :: fun((Grapheme | break, AccIn) -> AccOut),
           Grapheme :: string(),
           Acc0  :: any(),
           Acc1  :: any(),
           AccIn :: any(),
           AccOut :: any(),
           Text :: string().
grapheme_breaks(Fun, Acc, Text) -> gb_fold(Text, Fun, Acc).


-spec word_breaks(Fun, Acc0, Text) -> Acc1
      when Fun :: fun((Word | break, AccIn) -> AccOut),
           Word :: string(),
           Acc0  :: any(),
           Acc1  :: any(),
           AccIn :: any(),
           AccOut :: any(),
           Text :: string().
word_breaks(Fun, Acc, Text) -> wb_fold(Text, Fun, Acc).


-spec sentence_breaks(Fun, Acc0, Text) -> Acc1
      when Fun :: fun((Sentence | break, AccIn) -> AccOut),
           Sentence :: string(),
           Acc0  :: any(),
           Acc1  :: any(),
           AccIn :: any(),
           AccOut :: any(),
           Text :: string().
sentence_breaks(Fun, Acc, Text) -> sb_fold(Text, Fun, Acc).


-spec line_breaks(Fun, Acc0, Text) -> Acc1
      when Fun :: fun((Line | break, AccIn) -> AccOut),
           Line :: string(),
           Acc0  :: any(),
           Acc1  :: any(),
           AccIn :: any(),
           AccOut :: any(),
           Text :: string().
line_breaks(Fun, Acc, Text) -> lb_fold(Text, Fun, Acc).


-record(gws_state,{ segment
                  , prefix
                  , break
                  , out_fun
                  , out_acc}).

gb_fold([], _, Acc)     -> Acc;
gb_fold(Text, Fun, Acc) -> gb_fold_1(Text, gws_init(Fun, Acc)).

gb_fold_1([], State) -> gws_fini(State);
gb_fold_1(Cs, State) -> gb_start(Cs, State).


gb_start([C | Cs], State) ->
    gb_in(Cs, gws_advance(C, ucd_grapheme_break(C), State)).


gb_in([], State) ->
    gb_end([], State);

gb_in([C|Cs], #gws_state{prefix=G}=State) ->
    case ucd_grapheme_break(C) of
        lf when hd(G) == cr ->
            gb_end(Cs, gws_advance(C, State));

        _ when  hd(G) == control
              ; hd(G) == cr
              ; hd(G) == lf ->
            gb_end([C|Cs], State);

        GB when GB == control
              ; GB == cr
              ; GB == lf ->
            gb_end([C|Cs], State);

        GB when (       GB == l
                 orelse GB == v
                 orelse GB == lv
                 orelse GB == lvt
                )
              , hd(G) == l ->
            gb_in(Cs, gws_advance(C, GB, State));

        GB when (       GB == v
                 orelse GB == t
                )
              , (       hd(G) == lv
                 orelse hd(G) == v
                )->
            gb_in(Cs, gws_advance(C, GB, State));

        GB when GB == t
              , (       hd(G) == lvt
                 orelse hd(G) == t
                )->
            gb_in(Cs, gws_advance(C, GB, State));

        GB when GB == extend
              ; GB == zwj
              ; GB == spacing_mark ->
            gb_in(Cs, gws_advance(C, GB, State));

        GB when hd(G) == prepend ->
            gb_in(Cs, gws_advance(C, GB, State));

        e_modifier = GB ->
            gb_e_in(Cs, gws_advance(C, GB, State));

        GB when (       GB == glue_after_zwj
                 orelse GB == e_base_gaz
                )
              , hd(G) == zwj ->
            gb_in(Cs, gws_advance(C, GB, State));

        regional_indicator = GB ->
            gb_ri(Cs, gws_advance(C, GB, State));

        _ ->
            gb_end([C|Cs], State)
    end.


gb_ri(Cs, #gws_state{segment=S
                    ,prefix=[regional_indicator
                            ,regional_indicator
                            ,regional_indicator |_]}=State) ->
    gb_end([hd(S)|Cs], State#gws_state{segment=tl(S)});

gb_ri(Cs, #gws_state{prefix=[regional_indicator
                            ,regional_indicator |_]}=State) ->
    gb_in(Cs, State);

gb_ri(Cs, #gws_state{segment=S}=State) ->
    gb_end([hd(S)|Cs], State#gws_state{segment=tl(S)}).


gb_e_in(Cs, #gws_state{segment=[C|S], prefix=[_|G]}=State) ->
    case gb_check_e_base_extend(G) of
        true  -> gb_in(Cs, State);
        false -> gb_end([C|Cs], State#gws_state{segment=S})
    end.


gb_check_e_base_extend([G | _]) when G == e_base ; G == e_base_gaz -> true;
gb_check_e_base_extend([extend | Gs]) -> gb_check_e_base_extend(Gs);
gb_check_e_base_extend(_) -> false.


gb_end(Cs, State) ->
    gb_fold_1(Cs, gws_add_break(State)).


-define(AHLetter(WB),(       WB == a_letter
                      orelse WB == hebrew_letter)).

-define(MidNumLetQ(WB),(       WB == mid_num_let
                        orelse WB == single_quote)).

wb_fold([], _, Acc)     -> Acc;
wb_fold(Text, Fun, Acc) -> wb_fold_1(Text, gws_init(Fun, Acc)).

wb_fold_1([], State) -> gws_fini(State);
wb_fold_1(Cs, State) -> wb_start(Cs, State).


wb_start([], State) ->
    gws_fini(State);

wb_start([C | Cs], State) ->
    wb_start_1(Cs, gws_advance(C, ucd_word_break(C), State)).


wb_start_1([C2 | Cs], #gws_state{prefix=[WB1|_]}=State) ->
    wb_start_2(WB1, ucd_word_break(C2), C2, Cs, State);

wb_start_1([], State) ->
    gws_fini(State).


wb_start_2(cr, lf, C2, Cs, State) ->
    wb_start(Cs, gws_add_break(gws_advance(C2, State)));

wb_start_2(WB1, WB2, C2, Cs, State) when WB1 == newline
                                       ; WB1 == cr
                                       ; WB1 == lf ->
    wb_start_1(Cs, gws_advance(C2, WB2, gws_add_break(State)));

wb_start_2(zwj, WB2, C2, Cs, State) when WB2 == glue_after_zwj
                                       ; WB2 == e_base_gaz ->
    wb_start_1(Cs, gws_advance(C2, WB2, State));

wb_start_2(WB1, WB2, C2, Cs, State)  ->
    wb_in_2(WB1, WB2, C2, Cs, State).


wb_in_1([C2 | Cs], #gws_state{prefix=[WB1|_]}=State) ->
    wb_in_2(WB1, ucd_word_break(C2), C2, Cs, State);

wb_in_1([], State) ->
    gws_fini(State).


wb_in_2(_, WB2, C2, Cs, State) when WB2 == extend
                                  ; WB2 == format
                                  ; WB2 == zwj ->
    wb_in_1(Cs, gws_advance(C2, State));

wb_in_2(WB1, WB2, C2, Cs, State) when ?AHLetter(WB1), ?AHLetter(WB2) ->
    wb_in_1(Cs, gws_advance(C2, WB2, State));

wb_in_2(WB1, WB2, C2, Cs, State)
  when ?AHLetter(WB1), (WB2 == mid_letter orelse ?MidNumLetQ(WB2)) ->
    wb_in_ahl_mid(WB1, WB2, C2, Cs, State);

wb_in_2(WB1, WB2, C2, Cs, State)
  when WB1 == hebrew_letter, WB2 == double_quote ->
    wb_in_hl_dq(WB2, C2, Cs, State);

wb_in_2(WB1, WB2, C2, Cs, State) when WB1 == numeric, WB2 == numeric ->
    wb_in_1(Cs, gws_advance(C2, WB2, State));

wb_in_2(WB1, WB2, C2, Cs, State) when ?AHLetter(WB1), WB2 == numeric ->
    wb_in_1(Cs, gws_advance(C2, WB2, State));

wb_in_2(WB1, WB2, C2, Cs, State) when WB1 == numeric, ?AHLetter(WB2) ->
    wb_in_1(Cs, gws_advance(C2, WB2, State));

wb_in_2(WB1, WB2, C2, Cs, State)
  when  WB1 == numeric, (WB2 == mid_num orelse ?MidNumLetQ(WB2)) ->
    wb_in_num_mid(WB2, C2, Cs, State);

wb_in_2(WB1, WB2, C2, Cs, State) when WB1 == katakana, WB2 == katakana ->
    wb_in_1(Cs, gws_advance(C2, WB2, State));

wb_in_2(WB1, WB2, C2, Cs, State) when (?AHLetter(WB1)
                                       orelse WB1 == numeric
                                       orelse WB1 == katakana
                                       orelse WB1 == extend_num_let
                                      ), WB2 == extend_num_let ->
    wb_in_1(Cs, gws_advance(C2, WB2, State));

wb_in_2(WB1, WB2, C2, Cs, State) when WB1 == extend_num_let
                                    , (?AHLetter(WB2)
                                       orelse WB2 == numeric
                                       orelse WB2 == katakana) ->
    wb_in_1(Cs, gws_advance(C2, WB2, State));

wb_in_2(WB1, WB2, C2, Cs, State) when (WB1 == e_base orelse WB1 == e_base_gaz)
                                     , WB2 == e_modifier ->
    wb_in_1(Cs, gws_advance(C2, WB2, State));

wb_in_2(WB1, WB2, C2, Cs, State) when WB1 == regional_indicator
                                    , WB2 == regional_indicator ->
    wb_ri(Cs, gws_advance(C2, WB2, State));

wb_in_2(_, WB2, C2, Cs, State) ->
    wb_start_1(Cs, gws_advance(C2, WB2, gws_add_break(State))).


wb_in_ahl_mid(WB1, WB2, C2, Cs, State) ->
    case wb_in_ahl_mid_1(Cs, gws_advance(C2, WB2, State)) of
        {true, Cs1, State1} ->
            wb_in_1(Cs1, State1);

        false when WB1 == hebrew_letter
                 , WB2 == single_quote ->
            wb_in_1(Cs, gws_advance(C2, WB2, State));

        false ->
            wb_start_1(Cs, gws_advance(C2, WB2, gws_add_break(State)))
    end.

wb_in_ahl_mid_1([], _) ->
    false;

wb_in_ahl_mid_1([C | Cs], State) ->
    case ucd_word_break(C) of
         WB when WB == extend
               ; WB == format
               ; WB == zwj ->
            wb_in_ahl_mid_1(Cs, gws_advance(C, State));
        WB when ?AHLetter(WB) ->
            {true, Cs, gws_advance(C, WB, State)};
        _ ->
            false
    end.


wb_in_num_mid(WB2, C2, Cs, State) ->
    case wb_in_num_mid_1(Cs, gws_advance(C2, WB2, State)) of
        {true, Cs1, State1} ->
            wb_in_1(Cs1, State1);

        false ->
            wb_start_1(Cs, gws_advance(C2, WB2, gws_add_break(State)))
    end.

wb_in_num_mid_1([], _) ->
    false;

wb_in_num_mid_1([C | Cs], State) ->
    case ucd_word_break(C) of
         WB when WB == extend
               ; WB == format
               ; WB == zwj ->
            wb_in_num_mid_1(Cs, gws_advance(C, State));
        WB when WB == numeric ->
            {true, Cs, gws_advance(C, State)};
        _ ->
            false
    end.


wb_in_hl_dq(WB2, C2, Cs, State) ->
    case wb_in_hl_dq_1(Cs, gws_advance(C2, State)) of
        {true, Cs1, State1} -> wb_in_1(Cs1, State1);
        false -> wb_start_1(Cs, gws_advance(C2, WB2, gws_add_break(State)))
    end.

wb_in_hl_dq_1([], _) ->
    false;

wb_in_hl_dq_1([C | Cs], State) ->
    case ucd_word_break(C) of
         WB when WB == extend
               ; WB == format
               ; WB == zwj ->
            wb_in_hl_dq_1(Cs, gws_advance(C, State));
        WB when WB == hebrew_letter ->
            {true, Cs, gws_advance(C, WB, State)};
        _ ->
            false
    end.


wb_ri([], State) ->
    gws_fini(State);

wb_ri([C|Cs], State) ->
    case ucd_word_break(C) of
        WB when WB == extend
              ; WB == format
              ; WB == zwj ->
            wb_ri(Cs, gws_advance(C, State));

        WB ->
            wb_start_1(Cs, gws_advance(C, WB, gws_add_break(State)))
    end.


-define(ParaSep(SB),(       SB == sep
                     orelse SB == cr
                     orelse SB == lf)).

-define(SATerm(SB),(       SB == s_term
                    orelse SB == a_term)).

sb_fold([], _, Acc)     -> Acc;
sb_fold(Text, Fun, Acc) -> sb_fold_1(Text, gws_init(Fun, Acc)).

sb_fold_1([], State) -> gws_fini(State);
sb_fold_1(Cs, State) -> sb_start(Cs, State).


sb_start([], State) ->
    gws_fini(State);

sb_start([C | Cs], State) ->
    sb_start_1(Cs, gws_advance(C, ucd_sentence_break(C), State)).


sb_start_1([C2 | Cs], #gws_state{prefix=[SB1|_]}=State) ->
    sb_start_2(SB1, ucd_sentence_break(C2), C2, Cs, State);

sb_start_1([], State) ->
    gws_fini(State).


sb_start_2(cr, lf, C2, Cs, State) ->
    sb_start(Cs, gws_add_break(gws_advance(C2, State)));

sb_start_2(SB1, SB2, C2, Cs, State) when ?ParaSep(SB1) ->
    sb_start_1(Cs, gws_advance(C2, SB2, gws_add_break(State)));

sb_start_2(SB1, SB2, C2, Cs, State)  ->
    sb_in_2(SB1, SB2, C2, Cs, State).


sb_in_1([C2 | Cs], #gws_state{prefix=[SB1 | _]}=State) ->
    sb_in_2(SB1, ucd_sentence_break(C2), C2, Cs, State);

sb_in_1([], State) ->
    gws_fini(State).


sb_in_2(SB1, SB2, C2, Cs, State) when SB2 == extend
                                    ; SB2 == format ->
    if
        ?ParaSep(SB1) ->
            sb_start_1(Cs, gws_advance(C2, SB2, gws_add_break(State)));
        true ->
            sb_in_1(Cs, gws_advance(C2, State))
    end;

sb_in_2(SB1, SB2, C2, Cs, State) when SB1 == a_term
                                    , SB2 == numeric ->
    sb_in_1(Cs, gws_advance(C2, SB2, State));

sb_in_2(SB1, SB2, C2, Cs, State) when (       SB1 == upper
                                       orelse SB1 == lower
                                      ), SB2 == a_term ->
    case sb_in_ul_a_term(Cs, gws_advance(C2, SB2, State)) of
        {true, Cs1, State1} -> sb_in_1(Cs1, State1);
        false -> sb_in_1(Cs, gws_advance(C2, SB2, State))
    end;

sb_in_2(SB1, SB2, C2, Cs, State) when SB1 == a_term ->
    sb_in_a_term(SB2, C2, Cs, State);

sb_in_2(SB1, SB2, C2, Cs, State) when ?SATerm(SB1) ->
    sb_in_sa_term(SB2, C2, Cs, State);

sb_in_2(_, SB2, C2, Cs, State) ->
    sb_in_1(Cs, gws_advance(C2, SB2, State)).


sb_in_ul_a_term([C | Cs], State) ->
    case ucd_sentence_break(C) of
        upper = SB ->
            {true, Cs, gws_advance(C, SB, State)};

        SB when SB == extend
              ; SB == format ->
            sb_in_ul_a_term(Cs, gws_advance(C, State));

        _ ->
            false
    end;

sb_in_ul_a_term([], _) ->
    false.


sb_in_a_term(SB2, C2, Cs, State) ->
    sb_skip_close_sp(SB2, C2, Cs, State, fun sb_in_a_term_1/4).


sb_in_a_term_1(SB, C, [], State) when SB == lower
                                    ; SB == s_continue
                                    ; ?SATerm(SB)
                                    ; ?ParaSep(SB) ->
    gws_fini(gws_add_break(gws_advance(C, State)));

sb_in_a_term_1(_, C2, [], State) ->
    gws_fini(gws_add_break(gws_advance(C2, gws_add_break(State))));

sb_in_a_term_1(SB, C, Cs, State) ->
    case sb_in_a_term_2(SB, C, Cs, State) of
        {true, Cs1, State1} -> sb_in_1(Cs1, State1);
        false -> sb_in_sa_term_1(SB, C, Cs, State)
    end.


sb_in_a_term_2(SB, C, Cs, State) when SB == lower ->
    {true, Cs, gws_advance(C, SB, State)};

sb_in_a_term_2(SB1, C1, [C2|Cs], State) when SB1 == extend
                                           ; SB1 == format
                                           ; not (       SB1 == o_letter
                                                  orelse SB1 == upper
                                                  orelse SB1 == lower
                                                  orelse ?ParaSep(SB1)
                                                  orelse ?SATerm(SB1)) ->
    sb_in_a_term_2(ucd_sentence_break(C2), C2, Cs, gws_advance(C1, State));

sb_in_a_term_2(_S, _C, _Cs, _State) ->
    false.


sb_in_sa_term(SB2, C2, Cs, State) ->
    sb_skip_close_sp(SB2, C2, Cs, State, fun sb_in_sa_term_1/4).


sb_in_sa_term_1(SB2, C2, [], State) when SB2 == s_continue
                                       ; ?SATerm(SB2)
                                       ; ?ParaSep(SB2) ->
    gws_fini(gws_add_break(gws_advance(C2, State)));

sb_in_sa_term_1(_, C2, [], State) ->
    gws_fini(gws_add_break(gws_advance(C2, gws_add_break(State))));

sb_in_sa_term_1(SB2, C2, [C|Cs], State) when SB2 == s_continue
                                           ; ?SATerm(SB2) ->
    sb_in_1(Cs, gws_advance(C, ucd_sentence_break(C), gws_advance(C2, State)));

sb_in_sa_term_1(SB2, C2, [C|Cs], State) when ?ParaSep(SB2) ->
    sb_start_1(Cs, gws_advance(C, ucd_sentence_break(C), gws_add_break(gws_advance(C2, State))));

sb_in_sa_term_1(_, C2, [C|Cs], State) ->
    sb_start_1(Cs, gws_advance(C, ucd_sentence_break(C), gws_advance(C2,gws_add_break(State)))).


sb_skip_close_sp(SB2, C2, Cs, State, ContFun) ->
    sb_skip_close(SB2, C2, Cs, State, ContFun).

sb_skip_close(SB2, C2, [], State, _ContFun) when SB2 == close
                                               ; SB2 == sp
                                               ; SB2 == extend
                                               ; SB2 == format ->
    gws_fini(gws_add_break(gws_advance(C2, State)));

sb_skip_close(SB2, C2, [], State, ContFun) ->
    ContFun(SB2, C2, [], State);

sb_skip_close(SB2, C2, [C | Cs], State, ContFun) when SB2 == close
                                                    ; SB2 == extend
                                                    ; SB2 == format ->
    sb_skip_close(ucd_sentence_break(C), C, Cs, gws_advance(C2, State), ContFun);

sb_skip_close(SB2, C2, Cs, State, ContFun) ->
    sb_skip_sp(SB2, C2, Cs, State, ContFun).


sb_skip_sp(SB2, C2, [], State, _ContFun) when SB2 == sp
                                            ; SB2 == extend
                                            ; SB2 == format ->
    gws_fini(gws_add_break(gws_advance(C2, State)));

sb_skip_sp(SB2, C2, [], State, ContFun) ->
    ContFun(SB2, C2, [], State);

sb_skip_sp(SB2, C2, [C | Cs], State, ContFun) when SB2 == sp
                                                 ; SB2 == extend
                                                 ; SB2 == format ->
    sb_skip_sp(ucd_sentence_break(C), C, Cs, gws_advance(C2, State), ContFun);

sb_skip_sp(SB2, C2, Cs, State, ContFun) ->
    ContFun(SB2, C2, Cs, State).


gws_init(Fun, Acc) ->
    #gws_state{ segment = []
              , prefix = []
              , break = true
              , out_fun = Fun
              , out_acc = Fun(break, Acc)
              }.


gws_advance(C, Class, #gws_state{segment=S, prefix=P}=State) ->
    State#gws_state{segment=[C|S], prefix=[Class|P], break=false}.


gws_advance(C, #gws_state{segment=S}=State) ->
    State#gws_state{segment=[C|S], break=false}.


gws_add_break(#gws_state{break=true}=State) ->
    State;
gws_add_break(#gws_state{segment=[],out_fun=Fun,out_acc=Acc}=State) ->
    State#gws_state{prefix=[]
                   ,break=true
                   ,out_acc=Fun(break, Acc)
                   };
gws_add_break(#gws_state{segment=S,out_fun=Fun,out_acc=Acc}=State) ->
    State#gws_state{segment=[]
                   ,prefix=[]
                   ,break=true
                   ,out_acc=Fun(break, Fun(lists:reverse(S),Acc))
                   }.


gws_fini(State) ->
    #gws_state{out_acc=Acc} = gws_add_break(State), Acc.


lb_fold(Text, OutFun, OutAcc) ->
    lb_fold_1(lb_init(Text, OutFun, OutAcc)).


-record(lb_state,{segment
                 ,remaining
                 ,prefix
                 ,next
                 ,out_fun
                 ,out_acc
                 }).

lb_fold_1(StateIn) ->
    case lb_rules(StateIn) of
        #lb_state{next=eot, segment=[], out_acc=OutAcc} -> OutAcc;
        StateOut -> lb_fold_1(StateOut)
    end.


lb_init(Text, OutFun, OutAcc) ->
    #lb_state{segment   = []
             ,remaining = Text
             ,prefix    = [sot]
             ,next      = lb_next_class(Text)
             ,out_fun   = OutFun
             ,out_acc   = OutAcc
             }.


lb_rules(State) ->
    lb_rule2(State).

%% LB2: sot ×
lb_rule2(State) ->
    case lb_prefix(State) of
        sot -> lb_no_break(State);
        _   -> lb_rule3(State)
    end.


%% LB3: ! eot
lb_rule3(State) ->
    case lb_next(State) of
        eot -> lb_break(State);
        _   -> lb_rule4(State)
    end.


%% LB4: BK !
lb_rule4(State) ->
    case lb_prefix(State) of
        bk -> lb_break(State);
        _  -> lb_rule5(State)
    end.


%% LB5: CR × LF
%%      CR !
%%      LF !
%%      NL !
lb_rule5(State) ->
    case lb_prefix(State) of
        cr -> case lb_next(State) of
                  lf -> lb_no_break(State);
                  _  -> lb_break(State)
              end;
        lf -> lb_break(State);
        nl -> lb_break(State);
        _ ->  lb_rule6(State)
    end.


%% LB6: × ( BK | CR | LF | NL )
lb_rule6(State) ->
    case lb_next(State) of
        C when C == bk
             ; C == cr
             ; C == lf
             ; C == nl -> lb_no_break(State);
        _              -> lb_rule7(State)
    end.


%% LB7: × SP
%%      × ZW
lb_rule7(State) ->
    case lb_next(State) of
        C when  C == sp
              ; C == zw -> lb_no_break(State);
        _               -> lb_rule8(State)
    end.


%% LB8: ZW SP* ÷
lb_rule8(State) ->
    case lb_match_prefix([{some, sp}, zw], State) of
        true  -> lb_break(State);
        false -> lb_rule8a(State)
    end.


%% LB8a: ZWJ × (ID | EB | EM)
lb_rule8a(State) ->
    case lb_prefix(State) of
        zwj -> case lb_next(State) of
                   C when C == id
                          ; C == eb
                          ; C == em -> lb_no_break(State);
                   _                -> lb_rule9(State)
               end;
        _ -> lb_rule9(State)
    end.


%% LB9:  Treat X (CM | ZWJ)* as if it were X,  where X is any line break class
%%       except BK, CR, LF, NL, SP, or ZW.
lb_rule9(State) ->
    case lb_next(State) of
        C when C == cm
             ; C == zwj ->
            case lb_prefix(State) of
                X when X /= bk
                     , X /= cr
                     , X /= lf
                     , X /= nl
                     , X /= sp
                     , X /= zw
                     , X /= cm
                     , X /= zwj ->
                    lb_rules(lb_skip([cm, zwj], State));
                _  ->
                    lb_rule10(State)
            end;
        _ ->
            lb_rule10(State)
    end.


%% LB10: Treat any remaining CM or ZWJ as it if were AL.
lb_rule10(State) ->
    case lb_prefix(State) of
        C when C == cm
             ; C == zwj ->
            lb_rules(lb_skip([cm, zwj], lb_set_prefix(al, State)));
        _ ->
            lb_rule11(State)
    end.


%% LB11: × WJ
%%       WJ ×
lb_rule11(State) ->
    case lb_next(State) of
        wj ->
            lb_no_break(State);
        _ ->
            case lb_prefix(State) of
                wj -> lb_no_break(State);
                _  -> lb_rule12(State)
            end
    end.


%% LB12: GL ×
lb_rule12(State) ->
    case lb_prefix(State) of
        gl -> lb_no_break(State);
        _  -> lb_rule12a(State)
    end.


%% LB12a: [^SP BA HY] × GL
lb_rule12a(State) ->
    case lb_prefix(State) of
        C1 when C1 /= sp
              , C1 /= ba
              , C1 /= hy ->
            case lb_next(State) of
                C2 when C2 == gl -> lb_no_break(State);
                _                -> lb_rule13(State)
            end;
        _ ->
            lb_rule13(State)
    end.


%% LB13: × CL
%%       × CP
%%       × EX
%%       × IS
%%       × SY
lb_rule13(State) ->
    case lb_next(State) of
        C when  C == cl
              ; C == cp
              ; C == ex
              ; C == is
              ; C == sy -> lb_no_break(State);
        _               -> lb_rule14(State)
    end.


%% LB14: OP SP* ×
lb_rule14(State) ->
    case lb_match_prefix([{some, sp}, op], State) of
        true  -> lb_no_break(State);
        false -> lb_rule15(State)
    end.


%% LB15: QU SP* × OP
lb_rule15(State) ->
    case lb_next(State) of
        op ->
            case lb_match_prefix([{some,sp}, qu], State) of
                true  -> lb_no_break(State);
                false -> lb_rule16(State)
            end;
        _ ->
            lb_rule16(State)
    end.


%% LB16: (CL | CP) SP* × NS
lb_rule16(State) ->
    case lb_next(State) of
        ns ->
            case lb_match_prefix([{some, sp}, [cl, cp]], State) of
                true  -> lb_no_break(State);
                false -> lb_rule17(State)
            end;
        _ ->
            lb_rule17(State)
    end.


%% LB17: B2 SP* × B2
lb_rule17(State) ->
    case lb_next(State) of
        b2 ->
            case lb_match_prefix([{some,sp}, b2], State) of
                true  -> lb_no_break(State);
                false -> lb_rule18(State)
            end;
        _ ->
            lb_rule18(State)
    end.


%% LB18: SP ÷
lb_rule18(State) ->
    case lb_prefix(State) of
        sp -> lb_break(State);
        _  -> lb_rule19(State)
    end.


%% LB19: × QU
%%       QU ×
lb_rule19(State) ->
    case lb_next(State) of
        qu -> lb_no_break(State);
        _  -> case lb_prefix(State) of
                 qu -> lb_no_break(State);
                 _  -> lb_rule20(State)
              end
    end.


%% LB20: ÷ CB
%%       CB ÷
lb_rule20(State) ->
    case lb_next(State) of
        cb ->lb_break(State);
        _  -> case lb_prefix(State) of
                  cb -> lb_break(State);
                  _  -> lb_rule21(State)
              end
    end.


%% LB21: × BA
%%       × HY
%%       × NS
%%       BB ×
lb_rule21(State) ->
    case lb_next(State) of
        C when C == ba
             ; C == hy
             ; C == ns -> lb_no_break(State);

        _ -> case lb_prefix(State) of
                 bb -> lb_no_break(State);
                 _  -> lb_rule21a(State)
             end
    end.


%% LB21a: HL (HY | BA) ×
lb_rule21a(State) ->
    case lb_match_prefix([[hy,ba], hl],State) of
        true  -> lb_no_break(State);
        false -> lb_rule21b(State)
    end.


%% LB21b: SY × HL
lb_rule21b(State) ->
    case lb_prefix(State) of
        sy -> case lb_next(State) of
                  hl -> lb_no_break(State);
                  _  -> lb_rule22(State)
              end;
        _ -> lb_rule22(State)
    end.


%% LB22: (AL | HL) × IN
%%       EX × IN
%%       (ID | EB | EM) × IN
%%       IN × IN
%%       NU × IN
lb_rule22(State) ->
    case lb_prefix(State) of
        C1 when C1 == al
              ; C1 == hl
              ; C1 == ex
              ; C1 == id
              ; C1 == eb
              ; C1 == em
              ; C1 == in
              ; C1 == nu ->
            case lb_next(State) of
                in -> lb_no_break(State);
                 _ -> lb_rule23(State)
            end;
        _ ->
            lb_rule23(State)
    end.


%% LB23: (AL | HL) × NU
%%       NU × (AL | HL)
lb_rule23(State) ->
    case lb_prefix(State) of
        C1 when C1 == al
              ; C1 == hl ->
            case lb_next(State) of
                nu -> lb_no_break(State);
                 _ -> lb_rule23a(State)
            end;
        nu ->
            case lb_next(State) of
                C1 when C1 == al
                      ; C1 == hl -> lb_no_break(State);
                 _               -> lb_rule23a(State)
            end;
        _ ->
            lb_rule23a(State)
    end.


%% LB23a: PR × (ID | EB | EM)
%%        (ID | EB | EM) × PO
lb_rule23a(State) ->
    case lb_prefix(State) of
        pr ->
            case lb_next(State) of
                C1 when C1 == id
                      ; C1 == eb
                      ; C1 == em -> lb_no_break(State);
                 _               -> lb_rule24(State)
            end;
        C1 when C1 == id
              ; C1 == eb
              ; C1 == em ->
            case lb_next(State) of
                po -> lb_no_break(State);
                 _ -> lb_rule24(State)
            end;
        _ ->
            lb_rule24(State)
    end.


%% LB24: (PR | PO) × (AL | HL)
%%       (AL | HL) × (PR | PO)
lb_rule24(State) ->
    case lb_prefix(State) of
        C1 when C1 == pr
              ; C1 == po ->
            case lb_next(State) of
                C2 when C2 == al
                      ; C2 == hl -> lb_no_break(State);
                 _               -> lb_rule25(State)
            end;
        C1 when C1 == al
              ; C1 == hl ->
            case lb_next(State) of
                C2 when C2 == pr
                      ; C2 == po -> lb_no_break(State);
                 _               -> lb_rule25(State)
            end;
        _ ->
            lb_rule25(State)
    end.


lb_rule25(State) ->
    lb_rule25_1(State).

%% LB25: (PR | PO) × ( OP | HY )? NU
lb_rule25_1(State) ->
    case lb_prefix(State) of
        C when C == pr
             ; C == po ->
            case lb_match_suffix([{optional, [op, hy]}, nu], State) of
                true  -> lb_no_break(State);
                false -> lb_rule25_2(State)
            end;
        _ ->
            lb_rule25_2(State)
    end.

%%       ( OP | HY ) × NU
lb_rule25_2(State) ->
    case lb_prefix(State) of
        C when C == op
             ; C == hy ->
            case lb_next(State) of
                nu -> lb_no_break(State);
                _  -> lb_rule25_3(State)
            end;
        _ ->
            lb_rule25_3(State)
    end.

%%       NU × (NU | SY | IS)
lb_rule25_3(State) ->
    case lb_prefix(State) of
        nu ->
            case lb_next(State) of
                C when C == nu
                     ; C == sy
                     ; C == is -> lb_no_break(State);
                _              -> lb_rule25_4(State)
            end;
        _ ->
            lb_rule25_4(State)
    end.

%%       NU (NU | SY | IS)* × (NU | SY | IS | CL | CP )
lb_rule25_4(State) ->
    case lb_match_prefix([{some, [sy, is]}, nu], State) of
        true ->
            case lb_next(State) of
                C when C == nu
                     ; C == sy
                     ; C == is
                     ; C == cl
                     ; C == cp -> lb_no_break(State);
                _              -> lb_rule25_5(State)
            end;
        _ ->
            lb_rule25_5(State)
    end.

%%       NU (NU | SY | IS)* (CL | CP)? × (PO | PR)
lb_rule25_5(State) ->
    case lb_match_prefix([{optional,[cl,cp]}, {some, [sy, is]}, nu], State) of
        true  ->
            case lb_next(State) of
                C when C == po
                     ; C == pr -> lb_no_break(State);
                _              -> lb_rule26(State)
            end;
        false ->
            lb_rule26(State)
    end.


%% LB26: JL × (JL | JV | H2 | H3)
%%       (JV | H2) × (JV | JT)
%%       (JT | H3) × JT
lb_rule26(State) ->
    case lb_prefix(State) of
        jl ->
            case lb_next(State) of
                C when C == jl
                     ; C == jv
                     ; C == h2
                     ; C == h3 -> lb_no_break(State);
                _              -> lb_rule27(State)
            end;

        C when C == jv
             ; C == h2 ->
            case lb_next(State) of
                C1 when C1 == jv
                      ; C1 == jt -> lb_no_break(State);
                _                -> lb_rule27(State)
            end;

        C when C == jt
             ; C == h3 ->
            case lb_next(State) of
                jt -> lb_no_break(State);
                _  -> lb_rule27(State)
            end;

        _ ->
            lb_rule27(State)
    end.


%% LB27: (JL | JV | JT | H2 | H3) × IN
%%       (JL | JV | JT | H2 | H3) × PO
%%       PR × (JL | JV | JT | H2 | H3)
lb_rule27(State) ->
    case lb_next(State) of
        in ->
            case lb_prefix(State) of
                C when C == jl
                     ; C == jv
                     ; C == jt
                     ; C == h2
                     ; C == h3 -> lb_no_break(State);
                _              -> lb_rule28(State)
            end;

        po ->
            case lb_prefix(State) of
                C when C == jl
                     ; C == jv
                     ; C == jt
                     ; C == h2
                     ; C == h3 -> lb_no_break(State);
                _              -> lb_rule28(State)
            end;

        C when C == jl
             ; C == jv
             ; C == jt
             ; C == h2
             ; C == h3 ->
            case lb_prefix(State) of
                pr -> lb_no_break(State);
                _  -> lb_rule28(State)
            end;

        _ ->
            lb_rule28(State)
    end.


%% LB28: (AL | HL) × (AL | HL)
lb_rule28(State) ->
    case lb_prefix(State) of
        C1 when C1 == al
              ; C1 == hl ->
            case lb_next(State) of
                C2 when  C2 == al
                       ; C2 == hl -> lb_no_break(State);
                 _                -> lb_rule29(State)
            end;
        _ ->
            lb_rule29(State)
    end.


%% LB29: IS × (AL | HL)
lb_rule29(State) ->
    case lb_prefix(State) of
        is ->
            case lb_next(State) of
                C when C == al
                     ; C == hl -> lb_no_break(State);
                _              -> lb_rule30(State)
            end;
        _ ->
            lb_rule30(State)
    end.


%% LB30: (AL | HL | NU) × OP
%%       CP × (AL | HL | NU)
lb_rule30(State) ->
    case lb_prefix(State) of
        C1 when C1 == al
              ; C1 == hl
              ; C1 == nu ->
            case lb_next(State) of
                op -> lb_no_break(State);
                 _ -> lb_rule30a(State)
            end;
        cp ->
            case lb_next(State) of
                C1 when C1 == al
                      ; C1 == hl
                      ; C1 == nu -> lb_no_break(State);
                 _               -> lb_rule30a(State)
            end;
        _ ->
            lb_rule30a(State)
    end.


%% LB30a: sot (RI RI)* RI × RI
%%        [^RI] (RI RI)* RI × RI
lb_rule30a(State) ->
    case lb_match_prefix([ri, {some, ri, ri}, {'not', ri}], State) of
        true -> case lb_next(State) of
                    ri -> lb_no_break(State);
                    _  -> lb_rule30b(State)
                end;
        false -> lb_rule30b(State)
    end.


%% LB30b: EB × EM
lb_rule30b(State) ->
    case lb_prefix(State) of
       eb -> case lb_next(State) of
                 em -> lb_no_break(State);
                 _  -> lb_rule31(State)
             end;
        _ -> lb_rule31(State)
    end.


%% LB31: ALL ÷
%%       ÷ ALL
lb_rule31(State) ->
   lb_break(State).


lb_next_class([]) ->
    eot;
lb_next_class([C|_]) ->
    case ucd_line_break(C) of
        LB when LB == ai
              ; LB == sg
              ; LB == xx -> al;

        cj -> ns;

        sa ->
            case ucd_is_category(C, ['Mn', 'Mc']) of
                true  -> cm;
                false -> al
            end;

        LB -> LB
    end.


lb_break(#lb_state{segment=[], out_fun=Fun, out_acc=AccIn}=State) ->
    AccOut = Fun(break, AccIn),
    lb_advance(State#lb_state{out_acc=AccOut});

lb_break(#lb_state{segment=P, out_fun=Fun, out_acc=AccIn}=State) ->
    AccOut = Fun(break, Fun(lists:reverse(P), AccIn)),
    lb_advance(State#lb_state{segment=[],prefix=[],out_acc=AccOut}).


lb_no_break(State) ->
    lb_advance(State).


lb_prefix(#lb_state{prefix=[F|_]}) ->
    F.


lb_set_prefix(C, #lb_state{prefix=[_|T]}=State) ->
    State#lb_state{prefix=[C|T]}.


lb_match_prefix(Match, #lb_state{prefix=Prefix}) ->
    lb_match_prefix_1(Match, Prefix).

lb_match_prefix_1([], _) ->
    true;

lb_match_prefix_1([{'not', _}], []) ->
    true;

lb_match_prefix_1([{some, C} | _]=M, [C|Cs]) ->
    lb_match_prefix_1(M, Cs);

lb_match_prefix_1([{some, C1, C2} | _]=M, [C1, C2|Cs]) ->
    lb_match_prefix_1(M, Cs);

lb_match_prefix_1([{some, M} | Ms]=Ms1, [C|Cs]=Cs1) when is_list(M) ->
    case lists:member(C, M) of
        true  -> lb_match_prefix_1(Ms1, Cs);
        false -> lb_match_prefix_1(Ms, Cs1)
    end;

lb_match_prefix_1([{some, _, _} | M], P) ->
    lb_match_prefix_1(M, P);

lb_match_prefix_1([{some, _} | M], P) ->
    lb_match_prefix_1(M, P);

lb_match_prefix_1([{optional, C} | Ms], [C|Cs])  ->
    lb_match_prefix_1(Ms, Cs);

lb_match_prefix_1([{optional, Ms} | Ms1], [C|Cs]=Cs1) when is_list(Ms) ->
    case lists:member(C, Ms) of
        true  -> lb_match_prefix_1(Ms1, Cs);
        false -> lb_match_prefix_1(Ms1, Cs1)
    end;

lb_match_prefix_1([{optional, _} | M], P) ->
    lb_match_prefix_1(M, P);

lb_match_prefix_1([{'not', C} | _], [C | _]) ->
    false;

lb_match_prefix_1([{'not', _} | M], [_ | Cs]) ->
    lb_match_prefix_1(M, Cs);

lb_match_prefix_1([C | M], [C | Cs]) ->
    lb_match_prefix_1(M, Cs);

lb_match_prefix_1([M | Ms], [C | Cs]) when is_list(M) ->
    case lists:member(C, M) of
        true  -> lb_match_prefix_1(Ms, Cs);
        false -> false
    end;

lb_match_prefix_1(_, _) ->
    false.


lb_match_suffix([], _) ->
    true;


lb_match_suffix([{optional, C} | Ms], #lb_state{next=C}=State) ->
    lb_match_suffix(Ms, lb_advance(State));

lb_match_suffix([{optional, Cs} | Ms], #lb_state{next=N}=State)
  when is_list(Cs) ->
    case lists:member(N, Cs) of
        true  -> lb_match_suffix(Ms, lb_advance(State));
        false -> lb_match_suffix(Ms, State)
    end;

lb_match_suffix([{optional, _} | Ms], State) ->
    lb_match_suffix(Ms, State);


lb_match_suffix([C | Ms], #lb_state{next=C}=State) ->
    lb_match_suffix(Ms, lb_advance(State));

lb_match_suffix([Cs | Ms], #lb_state{next=N}=State) when is_list(Cs) ->
    case lists:member(N, Cs) of
        true  -> lb_match_suffix(Ms, lb_advance(State));
        false -> false
    end;

lb_match_suffix(_, _) ->
    false.


lb_skip(_, #lb_state{next=eot}=State) ->
    State;

lb_skip(Cs, #lb_state{segment=S,remaining=[H|T],next=N}=State) ->
    case lists:member(N, Cs) of
        true  ->
            lb_skip(Cs, State#lb_state{segment   = [H|S]
                                      ,remaining = T
                                      ,next      = lb_next_class(T)
                                      });
        false ->
            State
    end.


lb_next(#lb_state{next=N}) -> N.


lb_advance(#lb_state{next=eot}=State) ->
    State;

lb_advance(#lb_state{segment=S, remaining=[H|T], prefix=P, next=N}=State) ->
    State#lb_state{segment   = [H|S]
                  ,remaining = T
                  ,prefix    = [N|P]
                  ,next      = lb_next_class(T)
                  }.
