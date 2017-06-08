-module(unicodedata_case).
-include_lib("ucd/include/ucd.hrl").
-export([ is_lowercase/1
        , is_uppercase/1
        , is_cased/1
        , is_case_ignorable/1
        , case_folding/1
        , nfkc_casefold/1
        , to_uppercase/1
        , to_uppercase/2
        , to_lowercase/1
        , to_lowercase/2
        , to_titlecase/1
        , to_titlecase/2
        ]).


-spec is_lowercase(char()) -> boolean().
is_lowercase(CP) ->
    ucd:category(CP, 'Ll') orelse ucd:prop_list(CP, other_lowercase).


-spec is_uppercase(char()) -> boolean().
is_uppercase(CP) ->
    ucd:category(CP, 'Lu') orelse ucd:prop_list(CP, other_uppercase).


-spec is_cased(char()) -> boolean().
is_cased(CP) ->
    is_lowercase(CP) orelse is_uppercase(CP) orelse ucd:category(CP, 'Lt').


-spec is_case_ignorable(char()) -> boolean().
is_case_ignorable(CP) ->
    ucd:word_break_property(CP, [mid_letter, mid_num_let, single_quote])
        orelse ucd:category(CP, ['Mn', 'Me', 'Cf', 'Lm', 'Sk']).


-spec case_folding(char()) -> char() | string().
case_folding(CP) ->
    case ucd:full_case_folding(CP) of
        undefined ->
            case ucd:common_case_folding(CP) of
                undefined -> CP;
                V -> V
            end;
        V -> V
    end.


-spec nfkc_casefold(char()) -> char() | string().
nfkc_casefold(CP) ->
    case ucd:nfkc_casefold(CP) of
        undefined -> CP;
        V         -> V
    end.


-spec to_uppercase(string()) -> string().
to_uppercase(String) ->
    convert_case(String, fun uppercase_mapping/4, undefined).


-spec to_uppercase(string(), Lang :: binary()) -> string().
to_uppercase(String, Lang) ->
    convert_case(String, fun uppercase_mapping/4, Lang).


-spec to_lowercase(string()) -> string().
to_lowercase(String) ->
    convert_case(String, fun lowercase_mapping/4, undefined).


-spec to_lowercase(string(), Lang :: binary()) -> string().
to_lowercase(String, Lang) ->
    convert_case(String, fun lowercase_mapping/4, Lang).


-spec to_titlecase(string()) -> string().
to_titlecase(String) ->
    to_titlecase(String, undefined, [], []).


-spec to_titlecase(string(), Lang :: binary()) -> string().
to_titlecase(String, Lang) ->
    to_titlecase(String, Lang, [], []).


to_titlecase([], _, _, Acc) ->
    lists:reverse(Acc);

to_titlecase([CP|CPs], Lang, Prefix, Acc0) ->
    Prefix1 = [CP | Prefix],
    case is_cased(CP) of
        true ->
            Acc1 = case titlecase_mapping(CP, Prefix, CPs, Lang) of
                       Vs when is_list(Vs) -> push(Vs, Acc0);
                       V                   -> [V | Acc0]
                   end,
            convert_case(CPs, fun lowercase_mapping/4, Lang, Prefix1, Acc1);
        false ->
            to_titlecase(CPs, Lang, Prefix1, [CP | Acc0])
    end.


uppercase_mapping(CP, Prefix, Suffix, Lang) ->
    case ucd:special_uppercase(CP) of
        undefined ->
            simple_uppercase_mapping(CP);
        Vs ->
            case special_casing(Prefix, Suffix, Lang, Vs) of
                undefined -> simple_uppercase_mapping(CP);
                V         -> V
            end
    end.


simple_uppercase_mapping(CP) ->
    case ucd:uppercase(CP) of
        undefined -> CP;
        V         -> V
    end.


lowercase_mapping(CP, Prefix, Suffix, Lang) ->
    case ucd:special_lowercase(CP) of
        undefined ->
            simple_lowercase_mapping(CP);
        Vs ->
            case special_casing(Prefix, Suffix, Lang, Vs) of
                undefined -> simple_lowercase_mapping(CP);
                V         -> V
            end
    end.


simple_lowercase_mapping(CP) ->
    case ucd:lowercase(CP) of
        undefined -> CP;
        V         -> V
    end.


titlecase_mapping(CP, Prefix, Suffix, Lang) ->
    case ucd:special_titlecase(CP) of
        undefined ->
            simple_titlecase_mapping(CP);
        Vs ->
            case special_casing(Prefix, Suffix, Lang, Vs) of
                undefined -> simple_titlecase_mapping(CP);
                V         -> V
            end
    end.


simple_titlecase_mapping(CP) ->
    case ucd:titlecase(CP) of
        undefined -> CP;
        V         -> V
    end.


special_casing(_Prefix, _Suffix, _Lang, []) ->
    undefined;

special_casing(_Prefix, _Suffix, _Lang, [V | _]) when is_list(V) ->
    V;

special_casing(_Prefix, _Suffix, Lang, [{[Lang], V} | _]) ->
    V;

special_casing(Prefix, Suffix, Lang, [{[Lang, Ctx],V} | Vs]) ->
    case special_casing_context(Ctx, Prefix, Suffix) of
        true  -> V;
        false -> special_casing(Prefix, Suffix, Lang, Vs)
    end;

special_casing(Prefix, Suffix, Lang, [{[Ctx], V} | Vs]) when is_atom(Ctx) ->
    case special_casing_context(Ctx, Prefix, Suffix) of
        true  -> V;
        false -> special_casing(Prefix, Suffix, Lang, Vs)
    end;

special_casing(Prefix, Suffix, Lang, [_|Vs]) ->
    special_casing(Prefix, Suffix, Lang, Vs).


-define(COMBINING_DOT_ABOVE,16#0307).

special_casing_context(not_before_dot, Prefix, Suffix) ->
    not special_casing_context(before_dot, Prefix, Suffix);

special_casing_context(before_dot, _, Suffix) ->
    case skip_combining_classes_other_than_0_and_230(Suffix) of
        [?COMBINING_DOT_ABOVE | _] -> true;
        _                          -> false
    end;

special_casing_context(more_above, _, Suffix) ->
    case skip_combining_classes_other_than_0_and_230(Suffix) of
        [CP | _] -> ucd:combining_class(CP, 230);
        _        -> false
    end;

special_casing_context(after_I, Prefix, _) ->
    case skip_combining_classes_other_than_0_and_230(Prefix) of
        [$I | _] -> true;
        _        -> false
    end;

special_casing_context(after_soft_dotted, Prefix, _) ->
    case skip_combining_classes_other_than_0_and_230(Prefix) of
        [CP | _] -> ucd:prop_list(CP, soft_dotted);
        _        -> false
    end;

special_casing_context(final_sigma, Prefix, Suffix) ->
    case skip_case_ignorable(Prefix) of
        [CP1 | _] ->
            case is_cased(CP1) of
                true ->
                    case skip_case_ignorable(Suffix) of
                        [CP2 | _] -> not is_cased(CP2);
                        _         -> true
                    end;
                false ->
                    false
            end;
        _ ->
            false
    end;

special_casing_context(_, _, _) ->
    false.


convert_case(String, Fun, Lang) ->
    convert_case(String, Fun, Lang, [], []).


convert_case([], _, _, _, Acc) ->
    lists:reverse(Acc);

convert_case([CP|CPs], Fun, Lang, Prefix, AccIn) ->
    AccOut = case Fun(CP, Prefix, CPs, Lang) of
                 Vs when is_list(Vs) -> push(Vs, AccIn);
                 V                   -> [V | AccIn]
             end,
    convert_case(CPs, Fun, Lang, [CP | Prefix], AccOut).


skip_case_ignorable([]) ->
    [];

skip_case_ignorable([CP | CPs] = String) ->
    case is_case_ignorable(CP) of
        true  -> skip_case_ignorable(CPs);
        false -> String
    end.


skip_combining_classes_other_than_0_and_230([]) ->
    [];

skip_combining_classes_other_than_0_and_230([CP | CPs] = String) ->
    case ucd:combining_class(CP, not [0, 230]) of
        true  -> skip_combining_classes_other_than_0_and_230(CPs);
        false -> String
    end.


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

   ,?_assertEqual("I", to_uppercase("i"))
   ,?_assertEqual([16#0130], to_uppercase("i", <<"tr">>))

   ,?_assertEqual([$I, 16#307], to_uppercase([$i, 16#307]))
   ,?_assertEqual([$I],         to_uppercase([$i, 16#307], <<"lt">>))
   ,?_assertEqual([$I, 16#315], to_uppercase([$i, 16#315, 16#307], <<"lt">>))
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

   ,?_assertEqual("i",           to_lowercase("I"))
   ,?_assertEqual([16#0131],     to_lowercase("I", <<"tr">>))
   ,?_assertEqual([$i, 16#0307], to_lowercase([$I, 16#307]))
   ,?_assertEqual("i",           to_lowercase([$I, 16#307], <<"tr">>))

   ,?_assertEqual([$i, 16#300]        , to_lowercase([$I, 16#300]))
   ,?_assertEqual([$i, 16#307, 16#300], to_lowercase([$I, 16#300], <<"lt">>))

   % final sigma
   ,?_assertEqual([16#03C3],             to_lowercase([16#03A3]))
   ,?_assertEqual([$a, 16#03C2],         to_lowercase([$a, 16#03A3]))
   ,?_assertEqual([$a, $., 16#03C2, $.], to_lowercase([$a, $., 16#03A3, $.]))
   ,?_assertEqual([$a, 16#03C3, $., $a], to_lowercase([$a, 16#03A3, $., $a]))
].

to_titlecase_test_() -> [
    ?_assertEqual(" Abcd ", to_titlecase(" aBcD "))

   ,?_assertEqual([$F, $f, $ﬀ], to_titlecase([$ﬀ, $ﬀ]))

   ,?_assertEqual([$F, $i, $a], to_titlecase([$ﬁ, $a]))
].

-endif.
