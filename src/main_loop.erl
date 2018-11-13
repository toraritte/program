-module(main_loop).
-export([read_lines/1]).

read_lines(Regex) ->
    case io:get_line("") of
        eof ->
            no_op;
        Line ->
            Matched = match_line(Line, Regex),
            print_matching_lines(Matched),
            read_lines(Regex)
    end.

match_line(Line, Regex) ->
    Command = "echo '" ++ Line ++ "' | ./priv/bin/pcre2grep '" ++ Regex ++ "'",
    os:cmd(Command).

print_matching_lines([]) ->
    no_op;
print_matching_lines(Matched) ->
    io:format("~p~n", [Matched]).
