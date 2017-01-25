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
        , reorder/1
        , reorder/2
        ]).

-type category() :: unicodedata_properties:category().
-type bidirectional_class() :: unicodedata_bidirectional:bidirectional_class().
-type numeric() :: unicodedata_properties:numeric().
-type east_asian_width() :: unicodedata_properties:east_asian_width().


%% @doc Returns the general category property assigned to the character
-spec category(char()) -> category().
category(CP) -> unicodedata_properties:category(CP).


%% @doc Returns `true' if character is a letter with case
-spec is_cased_letter(char()) -> boolean().
is_cased_letter(CP) ->
    lists:member(category(CP), [ uppercase_letter
                               , lowercase_letter
                               , titlecase_letter
                               ]).


%% @doc Returns `true' if character is a letter
-spec is_letter(char()) -> boolean().
is_letter(CP) ->
    lists:member(category(CP), [ uppercase_letter
                               , lowercase_letter
                               , titlecase_letter
                               , modifier_letter
                               , other_letter
                               ]).


%% @doc Returns `true' if character is a spacing, monospacing or enclosing mark
-spec is_mark(char()) -> boolean().
is_mark(CP) ->
    lists:member(category(CP), [ monospacing_mark
                               , spacing_mark
                               , enclosing_mark
                               ]).


%% @doc Returns `true' if character represents a number
-spec is_number(char()) -> boolean().
is_number(CP) ->
    lists:member(category(CP), [ decimal_number
                               , letter_number
                               , other_number
                               ]).


%% @doc Returns `true' if character is a punctuation mark
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


%% @doc Returns `true' if character is a math, currency, modifier or other symbol
-spec is_symbol(char()) -> boolean().
is_symbol(CP) ->
    lists:member(category(CP), [ math_symbol
                               , currency_symbol
                               , modifier_symbol
                               , other_symbol
                               ]).


%% @doc Returns `true' if character is a space, line or paragraph separator
-spec is_separator(char()) -> boolean().
is_separator(CP) ->
    lists:member(category(CP), [ space_separator
                               , line_separator
                               , paragraph_separator
                               ]).


%% @doc Returns `true' if character is a control, format, surrogate, unassigned or for private use
-spec is_other(char()) -> boolean().
is_other(CP) ->
    lists:member(category(CP), [ control
                               , format
                               , surrogate
                               , private_use
                               , unassigned
                               ]).


%% @doc Returns `true' if codepoint is permanently reserved in the Unicode Standard for internal use
%%
%% `66 = length([CP || CP <- lists:seq(0, 16#10FFFF), unicodedata:is_noncharacter(CP)]).'
%%
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


%% @doc Returns `true' if codepoint is currently unassigned
-spec is_reserved(char()) -> boolean().
is_reserved(CP) ->
    case category(CP) of
        unassigned -> not is_noncharacter(CP);
        _          -> false
    end.


%% @doc Returns the directional type assigned to the character
-spec bidirectional_class(char()) -> bidirectional_class().
bidirectional_class(CP) ->
    unicodedata_bidirectional:bidirectional_class(CP).

%% @doc Returns `true' if character has strong bidirectional type
-spec is_bidirectional_strong(char()) -> boolean().
is_bidirectional_strong(CP) ->
    lists:member(bidirectional_class(CP), [ left_to_right
                                          , right_to_left
                                          , arabic_letter
                                          ]).


%% @doc Returns `true' if character has weak bidirectional type
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


%% @doc Returns `true' if character has neutral bidirectional type
-spec is_bidirectional_neutral(char()) -> boolean().
is_bidirectional_neutral(CP) ->
    lists:member(bidirectional_class(CP), [ paragraph_separator
                                          , segment_separator
                                          , white_space
                                          , other_neutral
                                          ]).


%% @doc Returns `true' if character is explicit directional formatting character
%%
%% The following characters signal that a piece of text has direction overridden,
%% treated as embedded or directionally isolated from its surroundings:
%% <table border="1" summary="explicit directional formatting characters">
%% <tr><th>Code Point</th><th>Name</th><th>Description</th></tr>
%% <tr><td>U+202A</td><td>LEFT-TO-RIGHT EMBEDDING</td>
%%     <td>Treat the following text as embedded left-to-right</td></tr>
%% <tr><td>U+202B</td><td>RIGHT-TO-LEFT EMBEDDING</td>
%%     <td>Treat the following text as embedded right-to-left</td></tr>
%% <tr><td>U+202D</td><td>LEFT-TO-RIGHT OVERRIDE</td>
%%     <td>Force following characters to be treated as strong left-to-right characters</td></tr>
%% <tr><td>U+202E</td><td>RIGHT-TO-LEFT OVERRIDE</td>
%%     <td>Force following characters to be treated as strong right-to-left characters</td></tr>
%%  <tr><td>U+202C</td><td>POP DIRECTIONAL FORMATTING</td>
%%     <td>End the scope of the last embedding or overriding</td></tr>
%%  <tr><td>U+2066</td><td>LEFT-TO-RIGHT ISOLATE</td>
%%     <td>Treat the following text as isolated and left-to-right</td></tr>
%%  <tr><td>U+2067</td><td>RIGHT-TO-LEFT ISOLATE</td>
%%     <td>Treat the following text as isolated and right-to-left</td></tr>
%%  <tr><td>U+2068</td><td>FIRST STRONG ISOLATE</td>
%%     <td>Treat the following text as isolated and in the direction of its
%%         first strong directional character that is not inside a nested isolate</td></tr>
%%  <tr><td>U+2069</td><td>POP DIRECTIONAL ISOLATE</td>
%%     <td>End the scope of the last directionally isolated text</td></tr>
%% </table>
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


%% @doc Returns `true' if character is identified as a 'mirrored' in bidirectional text
-spec is_mirrored(char()) -> boolean().
is_mirrored(CP) ->
    unicodedata_bidirectional:is_mirrored(CP).


%% @doc Returns corresponding 'mirrored' character
%%
%% ```
%% $] = unicodedata:mirroring_glyph($[).
%%
%% none = unicodedata:mirroring_glyph($a).
%% '''
-spec mirroring_glyph(char()) -> char() | none.
mirroring_glyph(CP) ->
    unicodedata_bidirectional:mirroring_glyph(CP).


%% @doc Returns the numeric type and value assigned to the character
%%
%% ```
%% {decimal, 0} = unicodedata:numeric($0).
%% {numeric, {1,4}} = unicodedata:numeric($¼).
%% not_a_number = unicodedata:numeric($a).
%% '''
-spec numeric(char()) -> {Type, Value} | not_a_number
      when Type :: numeric(),
           Value :: integer() | {integer(), pos_integer()}.
numeric(CP) -> unicodedata_properties:numeric(CP).


%% @doc Returns the numeric value assigned to the character
%%
%% ```
%% 9 = unicodedata:numeric_value($9, -1).
%% 0.25 = unicodedata:numeric_value($¼, -1).
%% not_a_number = unicodedata:numeric_value($a, not_a_number).
%% -1 = unicodedata:numeric_value($a, -1).
%% '''
-spec numeric_value(char(), Default) -> number() | Default
      when Default :: number() | not_a_number.
numeric_value(CP, Default) -> unicodedata_properties:numeric_value(CP, Default).


%% @doc Returns the east asian width property assigned to the character
%%
%% ```
%% narrow = unicodedata:east_asian_width($a).
%% neutral = unicodedata:east_asian_width($\n).
%% wide = unicodedata:east_asian_width(16#4E00).
%% '''
-spec east_asian_width(char()) -> east_asian_width().
east_asian_width(CP) -> unicodedata_properties:east_asian_width(CP).


%% @doc Returns the name assigned to the character
%%
%% Equivalent of calling `unicodedata:name(CP, standard)'.
-spec name(char()) -> binary().
name(CP) -> unicodedata_name:codepoint_name(CP).


%% @doc Returns the name assigned to the character
%%
%% `name(CP, standard)' returns the normative character name, as defined
%% in The Unicode Standard, control codes does not have defined character
%% names, so `<<>>' is returned.
%%
%% `name(CP, display)' returns character name useful for presentation.
%%
%% ```
%% <<>> = unicodedata:name($\r, standard).
%% <<"<CARRIAGE RETURN>">> = unicodedata:name($\r, display).
%% '''
-spec name(char(), standard | display) -> binary().
name(CP, standard) -> unicodedata_name:codepoint_name(CP);
name(CP, display)  -> unicodedata_name:codepoint_display_name(CP);
name(_, _)         -> error(badarg).


%% @doc Look up character by name
%%
%% ```
%% $a = unicodedata:lookup(<<"latin small letter a">>).
%% not_found = unicodedata:lookup(<<"?">>).
%% '''
-spec lookup(Name :: binary()) -> char() | not_found.
lookup(Name) -> unicodedata_name:codepoint_lookup(Name).


%% @doc Convert string to uppercase
%%
%% `"ABC" = unicodedata:to_uppercase("abc").'
-spec to_uppercase(string()) -> string().
to_uppercase(String) ->
    unicodedata_case:to_uppercase(String).


%% @doc Convert string to uppercase
%%
%% Use special rules for Lithuanian (`<<"lt">>'), Turkish (`<<"tr">>')
%% and Azeri `<<"az">>' languages.
%%
%% ```
%% [$I, 16#307] = unicodedata:to_uppercase([$i, 16#307], <<"en">>).
%% [$I]         = unicodedata:to_uppercase([$i, 16#307], <<"lt">>).
%% '''
-spec to_uppercase(string(), Lang :: binary()) -> string().
to_uppercase(String, Lang) ->
    unicodedata_case:to_uppercase(String, Lang).


%% @doc Convert string to lowercase
%%
%% `"abc" = unicodedata:to_lowercase("ABC").'
-spec to_lowercase(string()) -> string().
to_lowercase(String) ->
    unicodedata_case:to_lowercase(String).


%% @doc Convert string to lowercase
%%
%% Use special rules for Lithuanian (`<<"lt">>'), Turkish (`<<"tr">>')
%% and Azeri `<<"az">>' languages.
%%
%% ```
%% [$i, 16#0307] = unicodedata:to_lowercase([$I, 16#307], <<"en">>).
%% [$i]          = unicodedata:to_lowercase([$I, 16#307], <<"tr">>).
%% '''
-spec to_lowercase(string(), Lang :: binary()) -> string().
to_lowercase(String, Lang) ->
    unicodedata_case:to_lowercase(String, Lang).


%% @doc Make only first letter of each word capitalized
%%
%% `"Abc Def" = unicodedata:to_titlecase("aBc dEf").'
-spec to_titlecase(string()) -> string().
to_titlecase(String) ->
    do_titlecase(String, undefined).


%% @doc Make only first letter of each word capitalized
%%
%% Use special rules for Lithuanian (`<<"lt">>'), Turkish (`<<"tr">>')
%% and Azeri `<<"az">>' languages.
-spec to_titlecase(string(), Lang :: binary()) -> string().
to_titlecase(String, Lang) ->
    do_titlecase(String, Lang).


do_titlecase(String, Lang) ->
    Fun = fun (break, AccIn) -> AccIn;
              (W, AccIn)     -> [unicodedata_case:to_titlecase(W, Lang) | AccIn]
          end,
    Ws = unicodedata_segmentation:word_breaks(Fun, [], String),
    lists:flatten(lists:reverse(Ws)).


%% @doc Transform string into form used for caseless matching
%%
%% `"busse" = unicodedata:to_casefold("Buße").'
-spec to_casefold(string()) -> string().
to_casefold(String) ->
    Fun = fun (CP, Acc) -> insert(unicodedata_case:case_folding(CP), Acc) end,
    lists:reverse(lists:foldl(Fun, [], String)).


%% @doc Transform string into form used for caseless matching of identifiers
-spec to_nfkc_casefold(string()) -> string().
to_nfkc_casefold(String) ->
    Fun = fun (CP, Acc) -> insert(unicodedata_case:nfkc_casefold(CP), Acc) end,
    NfkcCasefold = lists:reverse(lists:foldl(Fun, [], String)),
    unicodedata_normalization:normalize(nfc, NfkcCasefold).


insert(V, Chars) when is_list(V) -> insert_1(V, Chars);
insert(V, Chars)                 ->  [V | Chars].

insert_1([], Chars)       -> Chars;
insert_1([C | Cs], Chars) -> insert_1(Cs, [C | Chars]).


%% @doc Compare two strings for case-insensitive equality
%%
%% Equivalent of calling `unicodedata:match(String1, String2, default)'.
%%
%% `true = unicodedata:match("BUSSE", "Buße").'
-spec match(string(), string()) -> boolean().
match(String1, String2) ->
    match(String1, String2, default).


%% @doc Compare two strings for case-insensitive equality
%%
%% Perform different variants of caseless matching as specified in
%% The Unicode Standard Section 3.13.
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


%% @doc Transform string into normalization form C
%%
%% Apply canonical decomposition, then compose pre-combined characters.
%%
%% `[$À] = unicodedata:to_nfc([$A, 16#300]).'
-spec to_nfc(string()) -> string().
to_nfc(String) -> unicodedata_normalization:normalize(nfc, String).


%% @doc Transform string into normalization form KC
%%
%% Apply compatibility decomposition, then compose pre-combined characters.
-spec to_nfkc(string()) -> string().
to_nfkc(String) -> unicodedata_normalization:normalize(nfkc, String).


%% @doc Transform string into normalization form D
%%
%% Translates each string character into its decomposed form.
%%
%% `[$A, 16#300] = unicodedata:to_nfd([$À]).'
-spec to_nfd(string()) -> string().
to_nfd(String) -> unicodedata_normalization:normalize(nfd, String).


%% @doc Transform string into normalization form KD
%%
%% Translates each string character into its decomposed form using
%% compatibility decomposition (all compatibility characters are replaced
%% with their equivalents).
-spec to_nfkd(string()) -> string().
to_nfkd(String) -> unicodedata_normalization:normalize(nfkd, String).


%% @doc Split string into grapheme clusters
%%
%% ``` [[$a, 16#308], [$b]] = unicodedata:graphemes([$a, 16#308, $b]).'''
-spec graphemes(string()) -> [string()].
graphemes(String) ->
    lists:reverse(graphemes(fun add_segment/2, [], String)).


%% @doc Split string into grapheme clusters
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


%% @doc Split string by word boundaries
%%
%% ``` ["ab"," ","cd"] = unicodedata:words("ab cd"). '''
-spec words(string()) -> [string()].
words(String) ->
    lists:reverse(words(fun add_segment/2, [], String)).


%% @doc Split string by word boundaries
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


%% @doc Split string by sentence boundaries
%%
%% ``` ["Ab?","Cd."] = unicodedata:sentences("Ab?Cd."). '''
-spec sentences(string()) -> [string()].
sentences(String) ->
    lists:reverse(sentences(fun add_segment/2, [], String)).


%% @doc Split string by sentence boundaries
-spec sentences(Fun, Acc0, string()) -> Acc1
      when Fun :: fun((Sentence, AccIn) -> AccOut),
           Sentence :: string(),
           Acc0 :: term(),
           Acc1 :: term(),
           AccIn :: term(),
           AccOut :: term().
sentences(Fun, Acc0, String) ->
    unicodedata_segmentation:sentence_breaks(fun (break, Acc) -> Acc;
                                                 (V, Acc)     -> Fun(V, Acc)
                                             end, Acc0, String).


%% @doc Split string by line boundaries
%%
%% ``` ["a\r\n", "b\n", "c"] = unicodedata:lines("a\r\nb\nc"). '''
-spec lines(string()) -> [string()].
lines(String) ->
    lists:reverse(lines(fun add_segment/2, [], String)).


%% @doc Split string by line boundaries
-spec lines(Fun, Acc0, string()) -> Acc1
      when Fun :: fun((Line, AccIn) -> AccOut),
           Line :: string(),
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


%% @doc Reorder text within paragraph for display
%%
%% Equivalent of calling
%%
%% ```unicodedata:reorder(String, [{line_breaks, [length(String)]}
%%                                ,{hide_explicit_directional_formatting, true}
%%                                ,{paragraph_direction, default}
%%                                ,{return, chars}
%%                                ]).
%% '''
-spec reorder(string()) -> string().
reorder(String) ->
    reorder(String, []).


%% @doc Reorder text within paragraph for display
%%
%% Execute Unicode Bidirectional Algorithm over a single paragraph as
%% described in Unicode Standard Annex #9.
%%
%% <table border="1" summary="paragraph reorder options">
%% <tr><th>Option</th><th>Description</th><th>Default</th></tr>
%% <tr><td>`line_breaks'</td>
%%     <td>If present the `line_breaks' list must contain at least one value.
%%         The values must be in strictly increasing order between 1 and the
%%         length of  the text. The last value must be equal to the length of
%%         the text.</td>
%%     <td>single line break at the paragraph end</td></tr>
%% <tr><td>`hide_explicit_directional_formatting'</td>
%%     <td>If `true' then remove all explicit directional formatting and
%%         boundary neutral characters from the result</td>
%%     <td>`true'</td></tr>
%% <tr><td>`paragraph_direction'</td>
%%     <td> If `default' is used then paragraph direction is automatically
%%          set based of paragraph content</td>
%%     <td>`default'</td></tr>
%% <tr><td>`return'</td>
%%     <td> Determine the function result value:<br/>
%%         `chars' - reordered characters<br/>
%%         `indices' - list of 0-based indices into original string</td>
%%     <td>`chars'</td></tr>
%% </table>
-spec reorder(string(), Options) -> string() | [non_neg_integer()]
      when Options :: [ {line_breaks, [non_neg_integer()]}
                      | {hide_explicit_directional_formatting, boolean()}
                      | {paragraph_direction, default
                                            | left_to_right
                                            | right_to_left}
                      | {return, chars | indices} ].
reorder(String, Options) ->
    P = case proplists:get_value(paragraph_direction, Options, default) of
            default ->
                unicodedata_bidirectional:paragraph(String);
            left_to_right ->
                unicodedata_bidirectional:paragraph(String, 0);
            right_to_left ->
                unicodedata_bidirectional:paragraph(String, 1);
            _ ->
                error({badarg, paragraph_direction})
        end,
    case proplists:get_value(return, Options, chars) of
        chars ->
            unicodedata_bidirectional:reorder(P, Options);
        indices ->
            unicodedata_bidirectional:reorder_indices(P, Options);
        _ ->
            error({badarg, return})
    end.


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

reorder_test_() -> [
    ?_assertEqual([$g, $h, $), $., $], $e, $f, $&, $[, 16#5D3, 16#5D2, $(,
                  16#5D1, 16#5D0],
                  reorder([16#5D0, 16#5D1,
                           $(, 16#5D2, 16#5D3, $[, $&, $e, $f, $], $., $),
                           $g, $h]))

   ,?_assertEqual([16#5D1, 16#5D0,
                   $(, 16#5D3, 16#5D2, $[, $&, $e, $f, $], $., $),
                   $g, $h],
                  reorder([16#5D0, 16#5D1,
                           $(, 16#5D2, 16#5D3, $[, $&, $e, $f, $], $., $),
                           $g, $h],
                         [{paragraph_direction, left_to_right}]))
].

-endif.
