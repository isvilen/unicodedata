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
        , bidirectional_class/1
        , is_bidirectional_strong/1
        , is_bidirectional_weak/1
        , is_bidirectional_neutral/1
        , is_bidirectional_explicit/1
        , is_mirrored/1
        , mirroring_glyph/1
        , numeric/1
        , numeric_value/2
        , east_asian_width/1
        , name/1
        , name/2
        , lookup/1
        , to_uppercase/1
        , to_uppercase/2
        , to_lowercase/1
        , to_lowercase/2
        , to_titlecase/1
        , to_titlecase/2
        , to_casefold/1
        , to_nfkc_casefold/1
        , match/2
        , match/3
        , to_nfc/1
        , to_nfkc/1
        , to_nfd/1
        , to_nfkd/1
        , graphemes/1
        , graphemes/3
        , words/1
        , words/3
        , sentences/1
        , sentences/3
        , lines/1
        , lines/3
        ]).

-type category() :: unicodedata_properties:category().
-type bidirectional_class() :: unicodedata_bidirectional:bidirectional_class().
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


-spec is_other(char()) -> boolean().
is_other(CP) ->
    lists:member(category(CP), [ control
                               , format
                               , surrogate
                               , private_use
                               , unassigned
                               ]).


-spec is_noncharacter(char()) -> boolean().
is_noncharacter(CP) ->
    case category(CP) of
        unassigned when CP >= 16#FDD0, CP =< 16#FDEF ->
            true;
        unassigned ->
            V = CP band 16#FFFF, V == 16#FFFE orelse V == 16#FFFF;
        _ ->
            false
    end.


-spec is_reserved(char()) -> boolean().
is_reserved(CP) ->
    case category(CP) of
        unassigned -> not is_noncharacter(CP);
        _          -> false
    end.


-spec bidirectional_class(char()) -> bidirectional_class().
bidirectional_class(CP) ->
    unicodedata_bidirectional:bidirectional_class(CP).


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
    unicodedata_bidirectional:is_mirrored(CP).


-spec mirroring_glyph(char()) -> char() | none.
mirroring_glyph(CP) ->
    unicodedata_bidirectional:mirroring_glyph(CP).


-spec numeric(char()) -> {Type, Value} | not_a_number
      when Type :: numeric(),
           Value :: integer() | {integer(), pos_integer()}.
numeric(CP) -> unicodedata_properties:numeric(CP).


-spec numeric_value(char(), Default) -> number() | Default
      when Default :: number() | not_a_number.
numeric_value(CP, Default) -> unicodedata_properties:numeric_value(CP, Default).


-spec east_asian_width(char()) -> east_asian_width().
east_asian_width(CP) -> unicodedata_properties:east_asian_width(CP).


-spec name(char()) -> binary().
name(CP) -> unicodedata_name:codepoint_name(CP).

-spec name(char(), standard | display) -> binary().
name(CP, standard) -> unicodedata_name:codepoint_name(CP);
name(CP, display)  -> unicodedata_name:codepoint_display_name(CP);
name(_, _)         -> error(badarg).


-spec lookup(Name :: binary()) -> char() | not_found.
lookup(Name) -> unicodedata_name:codepoint_lookup(Name).


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


-spec match(string(), string()) -> boolean().
match(String1, String2) ->
    match(String1, String2, default).


-spec match(string(), string(), Mode) -> boolean()
      when Mode :: default | canonical | compatibility | identifier.
match(String1, String2, default) ->
    to_casefold(String1) == to_casefold(String2);

match(String1, String2, canonical) ->
    to_nfd(to_casefold(to_nfd(String1))) == to_nfd(to_casefold(to_nfd(String2)));

match(String1, String2, compatibility) ->
       to_nfkd(to_casefold(to_nfkd(to_casefold(to_nfd(String1)))))
    == to_nfkd(to_casefold(to_nfkd(to_casefold(to_nfd(String2)))));

match(String1, String2, identifier) ->
    to_nfkc_casefold(to_nfd(String1)) == to_nfkc_casefold(to_nfd(String2));

match(_, _, _) ->
    error:badarg().


-spec to_nfc(string()) -> string().
to_nfc(String) -> unicodedata_normalization:normalize(nfc, String).

-spec to_nfkc(string()) -> string().
to_nfkc(String) -> unicodedata_normalization:normalize(nfkc, String).

-spec to_nfd(string()) -> string().
to_nfd(String) -> unicodedata_normalization:normalize(nfd, String).

-spec to_nfkd(string()) -> string().
to_nfkd(String) -> unicodedata_normalization:normalize(nfkd, String).


-spec graphemes(string()) -> [string()].
graphemes(String) ->
    lists:reverse(graphemes(fun add_segment/2, [], String)).

-spec graphemes(Fun, Acc0, string()) -> Acc1
      when Fun :: fun((Grapheme, AccIn) -> AccOut),
           Grapheme :: string(),
           Acc0 :: term(),
           Acc1 :: term(),
           AccIn :: term(),
           AccOut :: term().

graphemes(Fun, Acc0, String) ->
    unicodedata_segmentation:grapheme_breaks(fun (break, Acc) -> Acc;
                                                 (V, Acc)     -> Fun(V, Acc)
                                             end, Acc0, String).


-spec words(string()) -> [string()].
words(String) ->
    lists:reverse(words(fun add_segment/2, [], String)).

-spec words(Fun, Acc0, string()) -> Acc1
      when Fun :: fun((Word, AccIn) -> AccOut),
           Word :: string(),
           Acc0 :: term(),
           Acc1 :: term(),
           AccIn :: term(),
           AccOut :: term().

words(Fun, Acc0, String) ->
    unicodedata_segmentation:word_breaks(fun (break, Acc) -> Acc;
                                             (V, Acc)     -> Fun(V, Acc)
                                         end, Acc0, String).


-spec sentences(string()) -> [string()].
sentences(String) ->
    lists:reverse(sentences(fun add_segment/2, [], String)).

-spec sentences(Fun, Acc0, string()) -> Acc1
      when Fun :: fun((Word, AccIn) -> AccOut),
           Word :: string(),
           Acc0 :: term(),
           Acc1 :: term(),
           AccIn :: term(),
           AccOut :: term().

sentences(Fun, Acc0, String) ->
    unicodedata_segmentation:sentence_breaks(fun (break, Acc) -> Acc;
                                                 (V, Acc)     -> Fun(V, Acc)
                                             end, Acc0, String).



-spec lines(string()) -> [string()].
lines(String) ->
    lists:reverse(lines(fun add_segment/2, [], String)).

-spec lines(Fun, Acc0, string()) -> Acc1
      when Fun :: fun((Word, AccIn) -> AccOut),
           Word :: string(),
           Acc0 :: term(),
           Acc1 :: term(),
           AccIn :: term(),
           AccOut :: term().

lines(Fun, Acc0, String) ->
    unicodedata_segmentation:line_breaks(fun (break, Acc) -> Acc;
                                             (V, Acc)     -> Fun(V, Acc)
                                         end, Acc0, String).


add_segment(break, Acc) -> Acc;
add_segment(V, Acc)     -> [V | Acc].


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

bidirectional_class_test_() -> [
    ?_assert(is_bidirectional_strong($A))
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

match_test_() -> [
    ?_assert(match("Σίσυφος", "ΣΊΣΥΦΟΣ"))
   ,?_assert(match("BUSSE", "Buße"))
].

segmentation_test_() -> [
    ?_assertEqual([[$a, 16#308], [$b]], graphemes([$a, 16#308, $b]))

   ,?_assertEqual(["ab", " ", "cd", "\n", "ef"], words("ab cd\nef"))

   ,?_assertEqual(["Ab. ", "Cd\nef"], sentences("Ab. Cd\nef"))

   ,?_assertEqual(["a\r\n", "b\n", "c"], lines("a\r\nb\nc"))
].

-endif.
