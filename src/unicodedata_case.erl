-module(unicodedata_case).
-compile({parse_transform, ucd_transform}).
-export([ is_lowercase/1
        , is_uppercase/1
        , is_cased/1
        , is_case_ignorable/1
        , to_uppercase/1
        , to_lowercase/1
        ]).


-spec is_lowercase(char()) -> boolean().
is_lowercase(CP) ->
    ucd_is_category(CP, 'Ll') orelse ucd_has_property(CP, other_lowercase).


-spec is_uppercase(char()) -> boolean().
is_uppercase(CP) ->
    ucd_is_category(CP, 'Lu') orelse ucd_has_property(CP, other_uppercase).


-spec is_cased(char()) -> boolean().
is_cased(CP) ->
    is_lowercase(CP) orelse is_uppercase(CP) orelse ucd_is_category(CP, 'Lt').


-spec is_case_ignorable(char()) -> boolean().
is_case_ignorable(CP) ->
    ucd_word_break(CP, [mid_letter, mid_num_let, single_quote])
        orelse ucd_is_category(CP, ['Mn', 'Me', 'Cf', 'Lm', 'Sk']).


-spec to_uppercase(string()) -> string().
to_uppercase(String) ->
    convert_case(String, fun uppercase_mapping/1, []).


-spec to_lowercase(string()) -> string().
to_lowercase(String) ->
    convert_case(String, fun lowercase_mapping/1, []).


uppercase_mapping(CP) ->
    case ucd_special_casing(CP, upper) of
        {V, []} ->
            V;
        _ ->
            case ucd_uppercase_mapping(CP) of
                undefined -> CP;
                V         -> V
            end
    end.


lowercase_mapping(CP) ->
    case ucd_special_casing(CP, lower) of
        {V, []} ->
            V;
        _ ->
            case ucd_lowercase_mapping(CP) of
                undefined -> CP;
                V         -> V
            end
    end.

            
convert_case([], _, Acc) ->
    lists:reverse(Acc);

convert_case([CP|CPs], Fun, AccIn) ->
    AccOut = case Fun(CP) of
                 Vs when is_list(Vs) -> push(Vs, AccIn);
                 V                   -> [V | AccIn]
             end,
    convert_case(CPs, Fun, AccOut).


push([], Chars)       -> Chars;
push([C | Cs], Chars) -> push(Cs, [C | Chars]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_lowercase_test_() -> [
    ?_assert(is_lowercase($a))
   ,?_assert(is_lowercase(16#00AA))
   ,?_assert(is_lowercase(16#02B0))
   ,?_assert(is_lowercase(16#AB5C))

   ,?_assert(not is_lowercase($A))
].

is_uppercase_test_() -> [
    ?_assert(is_uppercase($A))
   ,?_assert(is_uppercase(16#2160))
   ,?_assert(is_uppercase(16#24B7))
   ,?_assert(is_uppercase(16#1F189))

   ,?_assert(not is_uppercase($a))
].

is_cased_test_() -> [
    ?_assert(is_cased($a))
   ,?_assert(is_cased($A))
   ,?_assert(is_cased(16#00B5))
   ,?_assert(is_cased(16#00BA))
   ,?_assert(is_cased(16#01C5))
   ,?_assert(is_cased(16#1F170))

   ,?_assert(not is_cased($0))
].

is_case_ignorable_test_() -> [
    ?_assert(is_case_ignorable(16#002E))
   ,?_assert(is_case_ignorable(16#005E))
   ,?_assert(is_case_ignorable(16#0060))
   ,?_assert(is_case_ignorable(16#00AF))
   ,?_assert(is_case_ignorable(16#02C0))
   ,?_assert(is_case_ignorable(16#0485))
   ,?_assert(is_case_ignorable(16#0488))
   ,?_assert(is_case_ignorable(16#061C))
   ,?_assert(is_case_ignorable(16#2018))
   ,?_assert(is_case_ignorable(16#2019))
   ,?_assert(is_case_ignorable(16#1F3FB))
   ,?_assert(is_case_ignorable(16#E0100))

   ,?_assert(not is_case_ignorable($a))
   ,?_assert(not is_case_ignorable($A))
].

to_uppercase_test_() -> [
    ?_assertEqual("BAFFLE",  to_uppercase("baﬄe"))
   ,?_assertEqual("STRASSE", to_uppercase("straße"))
   ,?_assertEqual("ÁÜÈSS",   to_uppercase("áüÈß"))

   ,?_assertEqual(             "123 ABCD 456 EFG HIJ"
                 ,to_uppercase("123 abcd 456 efg hij"))

   ,?_assertEqual(             "( %$#) KL MNOP @ QRST = -_ UVWXYZ"
                 ,to_uppercase("( %$#) kl mnop @ qrst = -_ uvwxyz"))

   ,?_assertEqual(             "& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ"
                 ,to_uppercase("& % # àáâ ãäå 1 2 ç æ"))

   ,?_assertEqual(             "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ"
                 ,to_uppercase("àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ"))
].

to_lowercase_test_() -> [
    ?_assertEqual("áüèß", to_lowercase("áüÈß"))

   ,?_assertEqual(             "123 abcd 456 efg hij"
                 ,to_lowercase("123 ABCD 456 EFG HIJ"))

   ,?_assertEqual(             "( %$#) kl mnop @ qrst = -_ uvwxyz"
                 ,to_lowercase("( %$#) KL MNOP @ QRST = -_ UVWXYZ"))

   ,?_assertEqual(             "& % # àáâ ãäå 1 2 ç æ"
                 ,to_lowercase("& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ"))

   ,?_assertEqual(             "àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ"
                 ,to_lowercase("ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ"))
].

-endif.
