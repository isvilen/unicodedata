-module(unicodedata).
-compile({parse_transform, unicodedata_ucd_transform}).
-export([ category/1
        , is_cased_letter/1
        , is_letter/1
        , is_mark/1
        , is_number/1
        , is_punctuation/1
        , is_symbol/1
        , is_separator/1
        , is_other/1
        , is_noncharacter/1
        , is_reserved/1
        , numeric/1
        , numeric_value/2
        , blocks/0
        , block/1
        , codepoint_block/1
        , to_uppercase/1
        , to_lowercase/1
        ]).

-type category() :: uppercase_letter
                  | lowercase_letter
                  | titlecase_letter

                  | modifier_letter
                  | other_letter

                  | monospacing_mark
                  | spacing_mark
                  | enclosing_mark

                  | decimal_number
                  | letter_number
                  | other_number

                  | connector_punctuation
                  | dash_punctuation
                  | open_punctuation
                  | close_punctuation
                  | initial_punctuation
                  | final_punctuation
                  | other_punctuation

                  | math_symbol
                  | currency_symbol
                  | modifier_symbol
                  | other_symbol

                  | space_separator
                  | line_separator
                  | paragraph_separator

                  | control
                  | format
                  | surrogate
                  | private_use
                  | unassigned.


-export_type([ category/0
             ]).


-spec category(char()) -> category().
category(CP) -> ucd_category(CP).


-spec is_cased_letter(char()) -> boolean().
is_cased_letter(CP) ->
    lists:member(ucd_category(CP), [ uppercase_letter
                                   , lowercase_letter
                                   , titlecase_letter
                                   ]).


-spec is_letter(char()) -> boolean().
is_letter(CP) ->
    lists:member(ucd_category(CP), [ uppercase_letter
                                   , lowercase_letter
                                   , titlecase_letter
                                   , modifier_letter
                                   , other_letter
                                   ]).


-spec is_mark(char()) -> boolean().
is_mark(CP) ->
    lists:member(ucd_category(CP), [ monospacing_mark
                                   , spacing_mark
                                   , enclosing_mark
                                   ]).


-spec is_number(char()) -> boolean().
is_number(CP) ->
    lists:member(ucd_category(CP), [ decimal_number
                                   , letter_number
                                   , other_number
                                   ]).


-spec is_punctuation(char()) -> boolean().
is_punctuation(CP) ->
    lists:member(ucd_category(CP), [ connector_punctuation
                                   , dash_punctuation
                                   , open_punctuation
                                   , close_punctuation
                                   , initial_punctuation
                                   , final_punctuation
                                   , other_punctuation
                                   ]).


-spec is_symbol(char()) -> boolean().
is_symbol(CP) ->
    lists:member(ucd_category(CP), [ math_symbol
                                   , currency_symbol
                                   , modifier_symbol
                                   , other_symbol
                                   ]).


-spec is_separator(char()) -> boolean().
is_separator(CP) ->
    lists:member(ucd_category(CP), [ space_separator
                                   , line_separator
                                   , paragraph_separator
                                   ]).


-spec is_other( char()) -> boolean().
is_other(CP) ->
    lists:member(ucd_category(CP), [ control
                                   , format
                                   , surrogate
                                   , private_use
                                   , unassigned
                                   ]).


-spec is_noncharacter( char()) -> boolean().
is_noncharacter(CP) ->
    case ucd_category(CP) of
        unassigned
          when CP >= 16#FDD0, CP =< 16#FDEF -> true;
        unassigned ->
            V = CP band 16#FFFF, V == 16#FFFE orelse V == 16#FFFF;
        _ ->
            false
    end.


-spec is_reserved( char()) -> boolean().
is_reserved(CP) ->
    case ucd_category(CP) of
        unassigned -> not is_noncharacter(CP);
        _          -> false
    end.


-spec numeric(char()) -> {Type, Value} | not_a_number
      when Type :: numeric | digit | decimal
                 | k_accounting_numeric | k_other_numeric | k_primary_numeric,
           Value :: integer() | {integer(), pos_integer()}.

numeric(CP) ->
    case ucd_numeric(CP) of
        undefined -> not_a_number;
        Value     -> Value
    end.


-spec numeric_value(char(), Default) -> number() | Default
      when Default :: number() | not_a_number.
numeric_value(CP, Default) ->
    case ucd_numeric(CP) of
        undefined     -> Default;
        {_, {V1, V2}} -> V1 / V2;
        {_, V}        -> V
    end.


-spec blocks() -> [{binary(), {char(), char()}}].
blocks() -> ucd_blocks().


-spec block(Name) -> {char(), char()} | no_block
      when Name :: binary() | string().
block(Name) when is_binary(Name) ->
    case lists:keyfind(Name, 1, blocks()) of
        {_, Range} -> Range;
        false      -> block(binary_to_list(Name))
    end;

block(Name) ->
    block(normalize_block_name(Name), blocks()).

block(_, []) ->
    no_block;

block(Name, [{Block, Range} | Blocks]) ->
    case normalize_block_name(binary_to_list(Block)) of
        Name -> Range;
        _    -> block(Name, Blocks)
    end.


-spec codepoint_block(char()) -> binary() | no_block.
codepoint_block(CP) -> ucd_block(CP).


-spec to_uppercase(string()) -> string().
to_uppercase(String) ->
    unicodedata_case:to_uppercase(String).


-spec to_lowercase(string()) -> string().
to_lowercase(String) ->
    unicodedata_case:to_lowercase(String).


normalize_block_name(Name) ->
    string:to_lower([C || C <- Name, C /= $\s, C /= $-, C /= $_]).


-ifdef(TEST).
-compile({no_auto_import,[is_number/1]}).
-include_lib("eunit/include/eunit.hrl").

category_test_() -> [
    ?_assertMatch(lowercase_letter,      category($a))
   ,?_assertMatch(uppercase_letter,      category($A))
   ,?_assertMatch(titlecase_letter,      category(16#01c5))
   ,?_assertMatch(modifier_letter,       category(16#02b0))
   ,?_assertMatch(other_letter,          category(16#00aa))
   ,?_assertMatch(monospacing_mark,      category(16#0300))
   ,?_assertMatch(spacing_mark,          category(16#0903))
   ,?_assertMatch(enclosing_mark,        category(16#1abe))
   ,?_assertMatch(decimal_number,        category($0))
   ,?_assertMatch(letter_number,         category(16#16ee))
   ,?_assertMatch(other_number,          category(16#00b2))
   ,?_assertMatch(connector_punctuation, category(16#203f))
   ,?_assertMatch(dash_punctuation,      category($-))
   ,?_assertMatch(open_punctuation,      category($[))
   ,?_assertMatch(close_punctuation,     category($]))
   ,?_assertMatch(initial_punctuation,   category($«))
   ,?_assertMatch(final_punctuation,     category($»))
   ,?_assertMatch(other_punctuation,     category($!))
   ,?_assertMatch(math_symbol,           category($+))
   ,?_assertMatch(currency_symbol,       category($$))
   ,?_assertMatch(modifier_symbol,       category($^))
   ,?_assertMatch(other_symbol,          category($¦))
   ,?_assertMatch(space_separator,       category($\s))
   ,?_assertMatch(line_separator,        category(16#2028))
   ,?_assertMatch(paragraph_separator,   category(16#2029))
   ,?_assertMatch(control,               category($\n))
   ,?_assertMatch(format,                category(16#00ad))
   ,?_assertMatch(surrogate,             category(16#d800))
   ,?_assertMatch(surrogate,             category(16#db80))
   ,?_assertMatch(surrogate,             category(16#dc00))
   ,?_assertMatch(private_use,           category(16#e000))


   ,?_assert(is_cased_letter($a))
   ,?_assert(is_cased_letter($A))
   ,?_assert(is_cased_letter(16#01c5))
   ,?_assert(not is_cased_letter(16#02b0))

   ,?_assert(is_letter($a))
   ,?_assert(is_letter($A))
   ,?_assert(is_letter(16#01c5))
   ,?_assert(is_letter(16#02b0))
   ,?_assert(is_letter(16#00aa))
   ,?_assert(not is_letter($0))

   ,?_assert(is_mark(16#0300))
   ,?_assert(is_mark(16#0903))
   ,?_assert(is_mark(16#1abe))
   ,?_assert(not is_mark($a))

   ,?_assert(is_number($0))
   ,?_assert(is_number(16#16ee))
   ,?_assert(is_number(16#00b2))
   ,?_assert(not is_number($A))

   ,?_assert(is_punctuation(16#203f))
   ,?_assert(is_punctuation($-))
   ,?_assert(is_punctuation(${))
   ,?_assert(is_punctuation($}))
   ,?_assert(is_punctuation(16#2017))
   ,?_assert(is_punctuation(16#2018))
   ,?_assert(is_punctuation(16#2019))
   ,?_assert(is_punctuation($!))
   ,?_assert(not is_punctuation($+))

   ,?_assert(is_symbol($+))
   ,?_assert(is_symbol($$))
   ,?_assert(is_symbol($^))
   ,?_assert(is_symbol($©))
   ,?_assert(not is_symbol($!))

   ,?_assert(is_separator($\s))
   ,?_assert(is_separator(16#2028))
   ,?_assert(is_separator(16#2029))
   ,?_assert(not is_separator($\n))

   ,?_assert(is_other($\n))
   ,?_assert(is_other(16#00ad))
   ,?_assert(not is_other($a))

   ,?_assert(is_noncharacter(16#FDD0))
   ,?_assert(is_noncharacter(16#FDEF))
   ,?_assert(is_noncharacter(16#FFFE))
   ,?_assert(is_noncharacter(16#FFFF))
   ,?_assert(is_noncharacter(16#10FFFE))
   ,?_assert(is_noncharacter(16#10FFFF))

   ,?_assert(is_reserved(16#E00FF))
].

numeric_test_() -> [
    ?_assertEqual({decimal, 9},      numeric(16#0f29))
   ,?_assertEqual({numeric, {1,2}},  numeric(16#0f2a))
   ,?_assertEqual({numeric, {17,2}}, numeric(16#0f32))
   ,?_assertEqual({numeric, {-1,2}}, numeric(16#0f33))
   ,?_assertEqual({decimal, 0},      numeric(16#1040))
   ,?_assertEqual({digit, 0},        numeric(16#2070))

   ,?_assertEqual({k_other_numeric, 5},              numeric(16#3405))
   ,?_assertEqual({k_primary_numeric, 10000},        numeric(16#4e07))
   ,?_assertEqual({k_accounting_numeric, 1000},      numeric(16#4edf))
   ,?_assertEqual({k_primary_numeric, 1000000000000},numeric(16#5146))
   ,?_assertEqual({k_primary_numeric, 100},          numeric(16#767e))
   ,?_assertEqual({k_accounting_numeric, 10000},     numeric(16#842c))
   ,?_assertEqual({k_other_numeric, 30},             numeric(16#20983))
   ,?_assertEqual({k_other_numeric, 40},             numeric(16#2098c))
   ,?_assertEqual({k_other_numeric, 4},              numeric(16#2626d))

   ,?_assertEqual(not_a_number, numeric($a))

   ,?_assertEqual(0,            numeric_value($a, 0))
   ,?_assertEqual(not_a_number, numeric_value($a, not_a_number))

   ,?_assertEqual(9,   numeric_value(16#0f29, 0))
   ,?_assertEqual(0.5, numeric_value(16#0f2a, 0))
   ,?_assertEqual(8.5, numeric_value(16#0f32, 0))
   ,?_assertEqual(-0.5,numeric_value(16#0f33, 0))
   ,?_assertEqual(0,   numeric_value(16#1040, -1))
   ,?_assertEqual(0,   numeric_value(16#2070, -1))

   ,?_assertEqual(1000000000000,     numeric_value(16#5146, 0))
].

blocks_test_() -> [
    ?_assertMatch([{<<"Basic Latin">>, {0,127}} | _], blocks())
   ,?_assertEqual({0,127}, block(<<"Basic Latin">>))
   ,?_assertEqual({16#100, 16#017F}, block("latin extended a"))
   ,?_assertEqual(no_block, block(<<"">>))
].

codepoint_block_test_() -> [
    ?_assertEqual(<<"Basic Latin">>,        codepoint_block(7))
   ,?_assertEqual(<<"Cyrillic">>,           codepoint_block(1029))
   ,?_assertEqual(<<"Letterlike Symbols">>, codepoint_block(8472))
   ,?_assertEqual(<<"Yi Syllables">>,       codepoint_block(41287))
   ,?_assertEqual(<<"Hangul Syllables">>,   codepoint_block(44032))
   ,?_assertEqual(<<"Variation Selectors">>,codepoint_block(65024))
   ,?_assertEqual(<<"Multani">>,            codepoint_block(70277))
   ,?_assertEqual(<<"CJK Unified Ideographs Extension B">>
                 ,codepoint_block(131072))

   ,?_assertEqual(<<"Low Surrogates">>,   codepoint_block(16#dc00))
   ,?_assertEqual(<<"Private Use Area">>, codepoint_block(16#e000))

   ,?_assertEqual(no_block, codepoint_block(16#E00FF))
].

-endif.
