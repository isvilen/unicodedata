-module(ucd_hangul).
-export([syllable_type/0]).

syllable_type() ->
    ucd:fold_lines(fun syllable_type_data/1, "HangulSyllableType.txt").

syllable_type_data([CpOrRange, Type]) ->
    {ucd:codepoint_or_range(CpOrRange), syllable_type(Type)}.

syllable_type(<<"L ">>)   -> l;
syllable_type(<<"V ">>)   -> v;
syllable_type(<<"T ">>)   -> t;
syllable_type(<<"LV ">>)  -> lv;
syllable_type(<<"LVT ">>) -> lvt.
