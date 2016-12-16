-module(unicodedata_bidirectional).
-compile({parse_transform, unicodedata_ucd_transform}).
-export([ bidirectional_class/1
        , is_bidirectional_strong/1
        , is_bidirectional_weak/1
        , is_bidirectional_neutral/1
        , is_bidirectional_explicit/1
        , is_bidirectional_mirrored/1
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

-export_type([bidirectional_class/0]).


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


-spec is_bidirectional_mirrored(char()) -> boolean().
is_bidirectional_mirrored(CP) ->
    ucd_bidi_mirrored(CP).


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

bidirectional_mirrored_test_() -> [
    ?_assert(is_bidirectional_mirrored($[)),
    ?_assert(is_bidirectional_mirrored($])),
    ?_assert(not is_bidirectional_mirrored($*))
].

-endif.
