-module(pattern_tests).
-include_lib("eunit/include/eunit.hrl").

to_regex_test_() ->
    [test_TCSs_without_modifier(),
     test_malformed_TCSs(),
     ?_assertEqual( "^text literal %{1$$", pattern:to_regex("text literal %{1$"))].

test_TCSs_without_modifier() ->
    [?_assertEqual("^\\b(.*)\\b$", pattern:to_regex("%{" ++ integer_to_list(N) ++ "}")) || N <- lists:seq(0,27)].

test_malformed_TCSs() ->
     % missing delimiters
    [?_assertEqual( "^%{1$", pattern:to_regex("%{1")),
     ?_assertEqual( "^%1}$", pattern:to_regex("%1}")),
     % non-existent modifier
     ?_assertEqual( "^%{X}$", pattern:to_regex("%{X}")),
     ?_assertEqual( "^%{1X}$", pattern:to_regex("%{1X}")),
     % no options with S modifier
     ?_assertEqual( "^%{1S}$", pattern:to_regex("%{1S}")),
     ?_assertEqual( "^%{S}$", pattern:to_regex("%{S}"))].
