-module(unicodedata).
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
        , east_asian_width/1
        , to_uppercase/1
        , to_uppercase/2
        , to_lowercase/1
        , to_lowercase/2
        , to_titlecase/1
        , to_titlecase/2
        , to_casefold/1
        , to_nfkc_casefold/1
        ]).

-type category() :: unicodedata_properties:category().
-type numeric() :: unicodedata_properties:numeric().
-type east_asian_width() :: unicodedata_properties:east_asian_width().


-spec category(char()) -> category().
category(CP) -> unicodedata_properties:category(CP).


-spec is_cased_letter(char()) -> boolean().
is_cased_letter(CP) ->
    lists:member(category(CP), [ uppercase_letter
                               , lowercase_letter
                               , titlecase_letter
                               ]).


-spec is_letter(char()) -> boolean().
is_letter(CP) ->
    lists:member(category(CP), [ uppercase_letter
                               , lowercase_letter
                               , titlecase_letter
                               , modifier_letter
                               , other_letter
                               ]).


-spec is_mark(char()) -> boolean().
is_mark(CP) ->
    lists:member(category(CP), [ monospacing_mark
                               , spacing_mark
                               , enclosing_mark
                               ]).


-spec is_number(char()) -> boolean().
is_number(CP) ->
    lists:member(category(CP), [ decimal_number
                               , letter_number
                               , other_number
                               ]).


-spec is_punctuation(char()) -> boolean().
is_punctuation(CP) ->
    lists:member(category(CP), [ connector_punctuation
                               , dash_punctuation
                               , open_punctuation
                               , close_punctuation
                               , initial_punctuation
                               , final_punctuation
                               , other_punctuation
                               ]).


-spec is_symbol(char()) -> boolean().
is_symbol(CP) ->
    lists:member(category(CP), [ math_symbol
                               , currency_symbol
                               , modifier_symbol
                               , other_symbol
                               ]).


-spec is_separator(char()) -> boolean().
is_separator(CP) ->
    lists:member(category(CP), [ space_separator
                               , line_separator
                               , paragraph_separator
                               ]).


-spec is_other( char()) -> boolean().
is_other(CP) ->
    lists:member(category(CP), [ control
                               , format
                               , surrogate
                               , private_use
                               , unassigned
                               ]).


-spec is_noncharacter( char()) -> boolean().
is_noncharacter(CP) ->
    case category(CP) of
        unassigned when CP >= 16#FDD0, CP =< 16#FDEF ->
            true;
        unassigned ->
            V = CP band 16#FFFF, V == 16#FFFE orelse V == 16#FFFF;
        _ ->
            false
    end.


-spec is_reserved( char()) -> boolean().
is_reserved(CP) ->
    case category(CP) of
        unassigned -> not is_noncharacter(CP);
        _          -> false
    end.


-spec numeric(char()) -> {Type, Value} | not_a_number
      when Type :: numeric(),
           Value :: integer() | {integer(), pos_integer()}.
numeric(CP) -> unicodedata_properties:numeric(CP).


-spec numeric_value(char(), Default) -> number() | Default
      when Default :: number() | not_a_number.
numeric_value(CP, Default) -> unicodedata_properties:numeric_value(CP, Default).


-spec east_asian_width(char()) -> east_asian_width().
east_asian_width(CP) -> unicodedata_properties:east_asian_width(CP).


-spec to_uppercase(string()) -> string().
to_uppercase(String) ->
    unicodedata_case:to_uppercase(String).


-spec to_uppercase(string(), Lang :: binary()) -> string().
to_uppercase(String, Lang) ->
    unicodedata_case:to_uppercase(String, Lang).


-spec to_lowercase(string()) -> string().
to_lowercase(String) ->
    unicodedata_case:to_lowercase(String).


-spec to_lowercase(string(), Lang :: binary()) -> string().
to_lowercase(String, Lang) ->
    unicodedata_case:to_lowercase(String, Lang).


-spec to_titlecase(string()) -> string().
to_titlecase(String) ->
    do_titlecase(String, undefined).


-spec to_titlecase(string(), Lang :: binary()) -> string().
to_titlecase(String, Lang) ->
    do_titlecase(String, Lang).


do_titlecase(String, Lang) ->
    Fun = fun (break, AccIn) -> AccIn;
              (W, AccIn)     -> [unicodedata_case:to_titlecase(W, Lang) | AccIn]
          end,
    Ws = unicodedata_segmentation:word_breaks(Fun, [], String),
    lists:flatten(lists:reverse(Ws)).


-spec to_casefold(string()) -> string().
to_casefold(String) ->
    Fun = fun (CP, Acc) -> insert(unicodedata_case:case_folding(CP), Acc) end,
    lists:reverse(lists:foldl(Fun, [], String)).


-spec to_nfkc_casefold(string()) -> string().
to_nfkc_casefold(String) ->
    Fun = fun (CP, Acc) -> insert(unicodedata_case:nfkc_casefold(CP), Acc) end,
    NfkcCasefold = lists:reverse(lists:foldl(Fun, [], String)),
    unicodedata_normalization:normalize(nfc, NfkcCasefold).


insert(V, Chars) when is_list(V) -> insert_1(V, Chars);
insert(V, Chars)                 ->  [V | Chars].

insert_1([], Chars)       -> Chars;
insert_1([C | Cs], Chars) -> insert_1(Cs, [C | Chars]).


-ifdef(TEST).
-compile({no_auto_import,[is_number/1]}).
-include_lib("eunit/include/eunit.hrl").

category_test_() -> [
    ?_assert(is_cased_letter($a))
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

to_titlecase_test_() -> [
    ?_assertEqual(" Abc Efg Ijh ", to_titlecase(" ABc eFG IjH "))
].

to_casefold_test_() -> [
    ?_assertEqual([16#3C5, 16#313, 16#300, $a, 16#FB],
                  to_casefold([16#1F52, $a, 16#DB]))
].

to_nfkc_casefold_test_() -> [
    ?_assertEqual("fax", to_nfkc_casefold([16#200B, 16#213B]))
].

-endif.
