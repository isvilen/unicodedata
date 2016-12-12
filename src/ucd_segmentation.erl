-module(ucd_segmentation).
-export([ grapheme_breaks/0
        , grapheme_breaks_classes/0
        , word_breaks/0
        , word_break_classes/0
        , sentence_breaks/0
        , sentence_break_classes/0
        , line_breaks/0
        , line_break_classes/0
        , non_tailorable_line_break_classes/0
        , line_breaks_defaults/0
        , line_break_class_name/1
        ]).


grapheme_breaks() ->
    Data = ucd:fold_lines(fun grapheme_break/1, "GraphemeBreakProperty.txt"),
    ucd:sort_by_codepoints(Data).

grapheme_breaks_classes() ->
    [cr, lf, control, extend, zwj, regional_indicator, prepend, spacing_mark,
     l, v, t, lv, lvt, e_base, e_modifier, glue_after_zwj, e_base_gaz].


word_breaks() ->
    Data = ucd:fold_lines(fun word_break/1, "WordBreakProperty.txt"),
    ucd:sort_by_codepoints(Data).

word_break_classes() ->
    [cr, lf, newline, extend, zwj, regional_indicator, format, katakana,
     hebrew_letter, a_letter, single_quote, double_quote, mid_num_let,
     mid_letter, mid_num, numeric, extend_num_let, e_base, e_modifier,
     glue_after_zwj, e_base_gaz].


sentence_breaks() ->
    Data = ucd:fold_lines(fun sentence_break/1, "SentenceBreakProperty.txt"),
    ucd:sort_by_codepoints(Data).

sentence_break_classes() ->
    [cr, lf, extend, sep, format, sp, lower, upper, o_letter, numeric, a_term,
     s_continue, s_term, close].


line_breaks() ->
    Data = ucd:fold_lines(fun line_break/1, "LineBreak.txt"),
    ucd:sort_by_codepoints(Data).


line_break_classes() -> [
    bk, cm, cr, gl, lf, nl, sg, sp, wj, zw, zwj, % non tailorable
    ai, al, b2, ba, bb, cb, cj, cl, cp, eb,
    em, ex, h2, h3, hl, hy, id, in, is, jl,
    jt, jv, ns, nu, op, po, pr, qu, ri, sa,
    sg, sy, xx
].

non_tailorable_line_break_classes() ->
    [bk, cm, cr, gl, lf, nl, sg, sp, wj, zw, zwj].


line_break_class_name(bk)  -> mandatory_break;
line_break_class_name(cm)  -> combining_mark;
line_break_class_name(cr)  -> carriage_return;
line_break_class_name(gl)  -> non_breaking_glue;
line_break_class_name(lf)  -> line_feed;
line_break_class_name(nl)  -> next_Line;
line_break_class_name(sg)  -> surrogate;
line_break_class_name(sp)  -> space;
line_break_class_name(wj)  -> word_joiner;
line_break_class_name(zw)  -> zero_width_space;
line_break_class_name(zwj) -> zero_width_joiner;
line_break_class_name(ai) -> ambiguous;
line_break_class_name(al) -> alphabetic;
line_break_class_name(b2) -> break_opportunity_before_and_after;
line_break_class_name(ba) -> break_after;
line_break_class_name(bb) -> break_before;
line_break_class_name(cb) -> contingent_break_opportunity;
line_break_class_name(cj) -> conditional_japanese_starter;
line_break_class_name(cl) -> close_punctuation;
line_break_class_name(cp) -> close_parenthesis;
line_break_class_name(eb) -> emoji_base;
line_break_class_name(em) -> emoji_modifier;
line_break_class_name(ex) -> exclamation;
line_break_class_name(h2) -> hangul_lv_syllable;
line_break_class_name(h3) -> hangul_lvt_syllable;
line_break_class_name(hl) -> hebrew_letter;
line_break_class_name(hy) -> hyphen;
line_break_class_name(id) -> ideographic;
line_break_class_name(in) -> inseparable;
line_break_class_name(is) -> infix_numeric_separator;
line_break_class_name(jl) -> hangul_l_jamo;
line_break_class_name(jt) -> hangul_t_jamo;
line_break_class_name(jv) -> hangul_v_jamo;
line_break_class_name(ns) -> nonstarter;
line_break_class_name(nu) -> numeric;
line_break_class_name(op) -> open_punctuation;
line_break_class_name(po) -> postfix_numeric;
line_break_class_name(pr) -> prefix_numeric;
line_break_class_name(qu) -> quotation;
line_break_class_name(ri) -> regional_indicator;
line_break_class_name(sa) -> complex_context_dependent;
line_break_class_name(sy) -> symbols_allowing_break_after;
line_break_class_name(xx) -> other.


line_breaks_defaults() -> [
    {{16#20A0, 16#20CF}, pr}
   ,{{16#3400, 16#4DBF}, id}
   ,{{16#4E00, 16#9FFF}, id}
   ,{{16#F900, 16#FAFF}, id}
   ,{{16#1F000, 16#1FFFD}, id}
   ,{{16#20000, 16#2FFFD}, id}
   ,{{16#30000, 16#3FFFD}, id}
].


grapheme_break([V,Break]) ->
    {ucd:codepoint_or_range(V), grapheme_break_class(Break)}.

grapheme_break_class(<<"CR ">>)                 -> cr;
grapheme_break_class(<<"LF ">>)                 -> lf;
grapheme_break_class(<<"Control ">>)            -> control;
grapheme_break_class(<<"Extend ">>)             -> extend;
grapheme_break_class(<<"ZWJ ">>)                -> zwj;
grapheme_break_class(<<"Regional_Indicator ">>) -> regional_indicator;
grapheme_break_class(<<"Prepend ">>)            -> prepend;
grapheme_break_class(<<"SpacingMark ">>)        -> spacing_mark;
grapheme_break_class(<<"L ">>)                  -> l;
grapheme_break_class(<<"V ">>)                  -> v;
grapheme_break_class(<<"T ">>)                  -> t;
grapheme_break_class(<<"LV ">>)                 -> lv;
grapheme_break_class(<<"LVT ">>)                -> lvt;
grapheme_break_class(<<"E_Base ">>)             -> e_base;
grapheme_break_class(<<"E_Modifier ">>)         -> e_modifier;
grapheme_break_class(<<"Glue_After_Zwj ">>)     -> glue_after_zwj;
grapheme_break_class(<<"E_Base_GAZ ">>)         -> e_base_gaz.


word_break([V,Break]) ->
    {ucd:codepoint_or_range(V), word_break_class(Break)}.

word_break_class(<<"CR ">>)                 -> cr;
word_break_class(<<"LF ">>)                 -> lf;
word_break_class(<<"Newline ">>)            -> newline;
word_break_class(<<"Extend ">>)             -> extend;
word_break_class(<<"ZWJ ">>)                -> zwj;
word_break_class(<<"Regional_Indicator ">>) -> regional_indicator;
word_break_class(<<"Format ">>)             -> format;
word_break_class(<<"Katakana ">>)           -> katakana;
word_break_class(<<"Hebrew_Letter ">>)      -> hebrew_letter;
word_break_class(<<"ALetter ">>)            -> a_letter;
word_break_class(<<"Single_Quote ">>)       -> single_quote;
word_break_class(<<"Double_Quote ">>)       -> double_quote;
word_break_class(<<"MidNumLet ">>)          -> mid_num_let;
word_break_class(<<"MidLetter ">>)          -> mid_letter;
word_break_class(<<"MidNum ">>)             -> mid_num;
word_break_class(<<"Numeric ">>)            -> numeric;
word_break_class(<<"ExtendNumLet ">>)       -> extend_num_let;
word_break_class(<<"E_Base ">>)             -> e_base;
word_break_class(<<"E_Modifier ">>)         -> e_modifier;
word_break_class(<<"Glue_After_Zwj ">>)     -> glue_after_zwj;
word_break_class(<<"E_Base_GAZ ">>)         -> e_base_gaz.


sentence_break([V,Break]) ->
    {ucd:codepoint_or_range(V), sentence_break_class(Break)}.

sentence_break_class(<<"CR ">>)        -> cr;
sentence_break_class(<<"LF ">>)        -> lf;
sentence_break_class(<<"Extend ">>)    -> extend;
sentence_break_class(<<"Sep ">>)       -> sep;
sentence_break_class(<<"Format ">>)    -> format;
sentence_break_class(<<"Sp ">>)        -> sp;
sentence_break_class(<<"Lower ">>)     -> lower;
sentence_break_class(<<"Upper ">>)     -> upper;
sentence_break_class(<<"OLetter ">>)   -> o_letter;
sentence_break_class(<<"Numeric ">>)   -> numeric;
sentence_break_class(<<"ATerm ">>)     -> a_term;
sentence_break_class(<<"SContinue ">>) -> s_continue;
sentence_break_class(<<"STerm ">>)     -> s_term;
sentence_break_class(<<"Close ">>)     -> close.


line_break([V,Break]) ->
    {ucd:codepoint_or_range(V), line_break_class(Break)}.

line_break_class(<<"BK ",_/binary>>)  -> bk;
line_break_class(<<"CM ",_/binary>>)  -> cm;
line_break_class(<<"CR ",_/binary>>)  -> cr;
line_break_class(<<"GL ",_/binary>>)  -> gl;
line_break_class(<<"LF ",_/binary>>)  -> lf;
line_break_class(<<"NL ",_/binary>>)  -> nl;
line_break_class(<<"SP ",_/binary>>)  -> sp;
line_break_class(<<"WJ ",_/binary>>)  -> wj;
line_break_class(<<"ZW ",_/binary>>)  -> zw;
line_break_class(<<"ZWJ ",_/binary>>) -> zwj;
line_break_class(<<"AI ",_/binary>>) -> ai;
line_break_class(<<"AL ",_/binary>>) -> al;
line_break_class(<<"B2 ",_/binary>>) -> b2;
line_break_class(<<"BA ",_/binary>>) -> ba;
line_break_class(<<"BB ",_/binary>>) -> bb;
line_break_class(<<"CB ",_/binary>>) -> cb;
line_break_class(<<"CJ ",_/binary>>) -> cj;
line_break_class(<<"CL ",_/binary>>) -> cl;
line_break_class(<<"CP ",_/binary>>) -> cp;
line_break_class(<<"EB ",_/binary>>) -> eb;
line_break_class(<<"EM ",_/binary>>) -> em;
line_break_class(<<"EX ",_/binary>>) -> ex;
line_break_class(<<"H2 ",_/binary>>) -> h2;
line_break_class(<<"H3 ",_/binary>>) -> h3;
line_break_class(<<"HL ",_/binary>>) -> hl;
line_break_class(<<"HY ",_/binary>>) -> hy;
line_break_class(<<"ID ",_/binary>>) -> id;
line_break_class(<<"IN ",_/binary>>) -> in;
line_break_class(<<"IS ",_/binary>>) -> is;
line_break_class(<<"JL ",_/binary>>) -> jl;
line_break_class(<<"JT ",_/binary>>) -> jt;
line_break_class(<<"JV ",_/binary>>) -> jv;
line_break_class(<<"NS ",_/binary>>) -> ns;
line_break_class(<<"NU ",_/binary>>) -> nu;
line_break_class(<<"OP ",_/binary>>) -> op;
line_break_class(<<"PO ",_/binary>>) -> po;
line_break_class(<<"PR ",_/binary>>) -> pr;
line_break_class(<<"QU ",_/binary>>) -> qu;
line_break_class(<<"RI ",_/binary>>) -> ri;
line_break_class(<<"SA ",_/binary>>) -> sa;
line_break_class(<<"SG ",_/binary>>) -> sg;
line_break_class(<<"SY ",_/binary>>) -> sy;
line_break_class(<<"XX ",_/binary>>) -> xx.
