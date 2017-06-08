-module(unicodedata_properties).
-include_lib("ucd/include/ucd.hrl").
-export([ category/1
        , numeric/1
        , numeric_value/2
        , east_asian_width/1
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


-type numeric() :: numeric
                 | digit
                 | decimal
                 | k_accounting_numeric
                 | k_other_numeric
                 | k_primary_numeric.


-type east_asian_width() :: ambiguous
                          | full_width
                          | half_width
                          | neutral
                          | narrow
                          | wide.

-export_type([ category/0
             , numeric/0
             , east_asian_width/0
             ]).


-spec category(char()) -> category().
category(CP) ->
    case ucd:category(CP) of
        'Lu' -> uppercase_letter;
        'Ll' -> lowercase_letter;
        'Lt' -> titlecase_letter;
        'Lm' -> modifier_letter;
        'Lo' -> other_letter;
        'Mn' -> monospacing_mark;
        'Mc' -> spacing_mark;
        'Me' -> enclosing_mark;
        'Nd' -> decimal_number;
        'Nl' -> letter_number;
        'No' -> other_number;
        'Pc' -> connector_punctuation;
        'Pd' -> dash_punctuation;
        'Ps' -> open_punctuation;
        'Pe' -> close_punctuation;
        'Pi' -> initial_punctuation;
        'Pf' -> final_punctuation;
        'Po' -> other_punctuation;
        'Sm' -> math_symbol;
        'Sc' -> currency_symbol;
        'Sk' -> modifier_symbol;
        'So' -> other_symbol;
        'Zs' -> space_separator;
        'Zl' -> line_separator;
        'Zp' -> paragraph_separator;
        'Cc' -> control;
        'Cf' -> format;
        'Cs' -> surrogate;
        'Co' -> private_use;
        'Cn' -> unassigned
    end.


-spec numeric(char()) -> {Type, Value} | not_a_number
      when Type :: numeric(),
           Value :: integer() | {integer(), pos_integer()}.

numeric(CP) ->
    case ucd:numeric(CP) of
        undefined -> not_a_number;
        Value     -> Value
    end.


-spec numeric_value(char(), Default) -> number() | Default
      when Default :: number() | not_a_number.
numeric_value(CP, Default) ->
    case ucd:numeric(CP) of
        undefined     -> Default;
        {_, {V1, V2}} -> V1 / V2;
        {_, V}        -> V
    end.


-spec east_asian_width(char()) -> east_asian_width().
east_asian_width(CP) ->
    ucd:east_asian_width(CP).


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

east_asian_width_test_() -> [
    ?_assertMatch(neutral,   east_asian_width(16#0000))
   ,?_assertMatch(narrow,    east_asian_width(16#0020))
   ,?_assertMatch(neutral,   east_asian_width(16#007f))
   ,?_assertMatch(ambiguous, east_asian_width(16#00a1))
   ,?_assertMatch(narrow,    east_asian_width(16#00a2))
   ,?_assertMatch(neutral,   east_asian_width(16#2011))
   ,?_assertMatch(ambiguous, east_asian_width(16#2013))
   ,?_assertMatch(half_width,east_asian_width(16#20a9))
   ,?_assertMatch(ambiguous, east_asian_width(16#25e2))
   ,?_assertMatch(neutral,   east_asian_width(16#25e6))
   ,?_assertMatch(wide,      east_asian_width(16#2e80))
   ,?_assertMatch(wide,      east_asian_width(16#a4c6))
   ,?_assertMatch(neutral,   east_asian_width(16#a4d0))
   ,?_assertMatch(wide,      east_asian_width(16#ac00))
   ,?_assertMatch(wide,      east_asian_width(16#d7a3))
   ,?_assertMatch(full_width,east_asian_width(16#ff01))
   ,?_assertMatch(half_width,east_asian_width(16#ff61))
   ,?_assertMatch(neutral,   east_asian_width(16#fff9))
   ,?_assertMatch(neutral,   east_asian_width(16#1f890))
   ,?_assertMatch(wide,      east_asian_width(16#1f9C0))
   ,?_assertMatch(wide,      east_asian_width(16#20000))
   ,?_assertMatch(neutral,   east_asian_width(16#e0001))
   ,?_assertMatch(ambiguous, east_asian_width(16#e0100))
].

-endif.
