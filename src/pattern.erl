-module(pattern).
-export([to_regex/1]).

% TODO: Fix  malformed TCS  matches. For  example, the
%       pattern below won't match:
% echo "foo blah is a %{1" | ./program "foo %{0} is a %{1}"
to_regex(Pattern) ->
    Tokens = string:lexemes(Pattern, " \n"),
    % io:format("~p~n", [Tokens]),
    Regexes = lists:map(fun token_to_regex/1, Tokens),
    % io:format("~p~n", [Regexes]),
    "^" ++ string:join(Regexes, " ") ++ "$".

% Checking the token capture sequence syntax one layer
% at a  time with  each subsequent  function. Warnings
% are emitted if the syntax check fails, and the token
% is treated as a regular text literal.
%
% 1. `token_to_regex` checks for possible TCSs
%
% 2. `tcs_to_regex` possible TCSs for valid delimiters
%    (`%{` and `}`)
%
% 3. `parse_tcs_internals` checks  base (i.e., `%{N}`)
%    and G modifier cases.  Offloads S modifier syntax
%    check to
%
% 4. `parse_s_modifier`  and  produces  regex  if  TCS
%    syntax is valid.
token_to_regex([$%, ${|_] = PossibleTCS) ->
    % io:format("~p~n", [PossibleTCS]),
    tcs_to_regex(PossibleTCS);
token_to_regex({malformed_tcs, Literal}) ->
    Literal;
token_to_regex(Literal) ->
    Literal.

malformed_tcs(Hint, Token) ->
    Warning =
        "Warning: " ++ tcs_hint(Hint) ++
        " Treating ~p as text literal.\n\n",

    io:format(standard_error, Warning, [Token]),
    token_to_regex({malformed_tcs, Token}).

tcs_hint(delimiter_missing) ->
    "Token capture sequence possibly missing closing \"}\".";
tcs_hint(first_comp_not_integer) ->
    "The first component of the token capture sequence must be a non-negative-integer.";
tcs_hint(first_comp_negative) ->
    tcs_hint(first_comp_not_integer);
tcs_hint(unknown_modifier) ->
    "Token capture sequences only support the S and G modifiers.";
tcs_hint(missing_s_modifier_option) ->
    "The token capture sequence's S modifier requires a non-negative integer following the modifier. Extra characters will be considered non-conforming.";
tcs_hint(non_integer_s_modifier_option) ->
    tcs_hint(missing_s_modifier_option);
tcs_hint(negative_integer_s_modifier_option) ->
    tcs_hint(missing_s_modifier_option);
tcs_hint(extra_chars_after_s_modifier_option) ->
    tcs_hint(missing_s_modifier_option).

% Cannot simply  pattern-match because the input  is a
% string and, according to  the syntax spec, the first
% part has to be a non-negative integer.
tcs_to_regex([$%, ${ | Rest] = PossibleTCS) ->
    case lists:reverse(Rest) of
        % Or use `lists:last/1`.
        [$}|T] ->
            TCSInternals = lists:reverse(T),
            % io:format("TCSInternals: ~p~n", [TCSInternals]),
            parse_tcs_internals(TCSInternals, PossibleTCS);
        _ ->
            malformed_tcs(delimiter_missing, PossibleTCS)
    end.

parse_tcs_internals(Inner, Original) ->
    BaseTCSRegex = "\\b(.*)\\b",

    % Ignoring the  TCS integer  for now  because querying
    % the captures is not  specified, but making sure that
    % the TCS  syntax is correct.  That is, the  first TCS
    % component is a non-negative integer.
    case string:to_integer(Inner) of
        {error, _} ->
            malformed_tcs(first_comp_not_integer, Original);
        {Int, _} when Int < 0 ->
            malformed_tcs(first_comp_negative, Original);
        {_, []} ->
            BaseTCSRegex;
        {_, [$G]} ->
            BaseTCSRegex;
        {_, [$S|_]} ->
            parse_s_modifier(Inner, Original);
        _ ->
            malformed_tcs(unknown_modifier, Original)
    end.

parse_s_modifier([_, $S | []], Original) ->
    malformed_tcs(missing_s_modifier_option, Original);
parse_s_modifier([_, $S | Limiter], Original) ->
    case string:to_integer(Limiter) of
        {error, _} ->
            malformed_tcs(non_integer_s_modifier_option, Original);
        {Int, []} when Int < 0 ->
            malformed_tcs(negative_integer_s_modifier_option, Original);
        {Int, []} ->
            make_s_regex(Int);
        _ ->
            malformed_tcs(extra_chars_after_s_modifier_option, Original)
    end.

make_s_regex(Int) ->
    "\\b(" ++ do_make_s_regex(Int) ++ ")\\b".

do_make_s_regex(0) ->
    "\\S+";
do_make_s_regex(Int) ->
    do_make_s_regex(Int-1) ++ "\\s{1}\\S+".
