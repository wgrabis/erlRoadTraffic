%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Macros used in model_test
%%% @end
%%%-------------------------------------------------------------------
-author("Jakub Kudzia").

-define(TEST_DIR, filename:dirname(?FILE)).
-define(TEST_JSON, filename:join([?TEST_DIR, "test.json"])).
-define(TEST_JSON2, filename:join([?TEST_DIR, "test2.json"])).
-define(TEST_JSON3, filename:join([?TEST_DIR, "test3.json"])).

-define(TEST_GRAPH1, #{
    <<"A">> => [<<"N2">>],
    <<"B">> => [],
    <<"N1">> => [<<"A">>, <<"B">>],
    <<"N2">> => [<<"V">>, <<"A">>],
    <<"V">> => [<<"N1">>]
}).

-define(TEST_GRAPH1_TRANSP, #{
    <<"A">> => [<<"N1">>, <<"N2">>],
    <<"B">> => [<<"N1">>],
    <<"N1">> => [<<"V">>],
    <<"N2">> => [<<"A">>],
    <<"V">> => [<<"N2">>]
}).

-define(FILTERED_TEST_GRAPH1, #{
    <<"A">> => [<<"N2">>],
    <<"B">> => [],
    <<"N1">> => [<<"A">>, <<"B">>],
    <<"N2">> => [<<"N1">>, <<"A">>]
}).

-define(FILTERED_TEST_GRAPH1_TRANSP, #{
    <<"A">> => [<<"N1">>, <<"N2">>],
    <<"B">> => [<<"N1">>],
    <<"N1">> => [<<"N2">>],
    <<"N2">> => [<<"A">>]
}).

-define(TEST_GRAPH2, #{
    <<"A">> => [<<"N1">>],
    <<"B">> => [],
    <<"N1">> => [<<"V">>, <<"B">>, <<"A">>],
    <<"N2">> => [<<"V">>, <<"A">>],
    <<"V">> => [<<"N1">>, <<"N2">>]
}).

-define(TEST_GRAPH2_TRANSP, #{
    <<"A">> => [<<"N2">>, <<"N1">>],
    <<"B">> => [<<"N1">>],
    <<"N1">> => [<<"V">>, <<"A">>],
    <<"N2">> => [<<"V">>],
    <<"V">> => [<<"N1">>, <<"N2">>]
}).

-define(FILTERED_TEST_GRAPH2, #{
    <<"A">> => [<<"N1">>],
    <<"B">> => [],
    <<"N1">> => [<<"N2">>, <<"B">>, <<"A">>],
    <<"N2">> => [<<"N1">>, <<"A">>]
}).

-define(FILTERED_TEST_GRAPH2_TRANSP, #{
    <<"A">> => [<<"N1">>, <<"N2">>],
    <<"B">> => [<<"N1">>],
    <<"N1">> => [<<"A">>, <<"N2">>],
    <<"N2">> => [<<"N1">>]
}).

-define(TEST_GRAPH3, #{
    <<"A">> => [<<"N1">>],
    <<"B">> => [],
    <<"N1">> => [<<"V">>, <<"B">>],
    <<"N2">> => [<<"V">>, <<"A">>],
    <<"V">> => [<<"N1">>, <<"N2">>]
}).

-define(TEST_GRAPH3_TRANSP, #{
    <<"A">> => [<<"N2">>],
    <<"B">> => [<<"N1">>],
    <<"N1">> => [<<"V">>, <<"A">>],
    <<"N2">> => [<<"V">>],
    <<"V">> => [<<"N1">>, <<"N2">>]
}).

-define(FILTERED_TEST_GRAPH3, #{
    <<"B">> => [],
    <<"N1">> => [<<"N2">>, <<"B">>],
    <<"N2">> => [<<"N1">>]
}).

-define(FILTERED_TEST_GRAPH3_TRANSP, #{
    <<"B">> => [<<"N1">>],
    <<"N1">> => [<<"N2">>],
    <<"N2">> => [<<"N1">>]
}).
