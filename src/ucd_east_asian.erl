-module(ucd_east_asian).
-export([ east_asian_width/0
        , east_asian_width_values/0
        , east_asian_width_defaults/0
        ]).

east_asian_width() ->
    ucd:fold_lines(fun east_asian_width_data/1, "EastAsianWidth.txt").


east_asian_width_values() ->
    [ambiguous, full_width, half_width, neutral, narrow, wide].


east_asian_width_defaults() -> [
    {{16#3400, 16#4DBF}, wide}
   ,{{16#4E00, 16#9FFF}, wide}
   ,{{16#F900, 16#FAFF}, wide}
   ,{{16#20000, 16#2FFFD}, wide}
   ,{{16#30000, 16#3FFFD}, wide}
].


east_asian_width_data([CpOrRange, V]) ->
    {ucd:codepoint_or_range(CpOrRange), east_asian_width_value(V)}.


east_asian_width_value(<<"A ",_/binary>>) -> ambiguous;
east_asian_width_value(<<"F ",_/binary>>) -> full_width;
east_asian_width_value(<<"H ",_/binary>>) -> half_width;
east_asian_width_value(<<"N ",_/binary>>) -> neutral;
east_asian_width_value(<<"Na ",_/binary>>)-> narrow;
east_asian_width_value(<<"W ",_/binary>>) -> wide.
