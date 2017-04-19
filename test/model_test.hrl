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

-include("model.hrl").

-define(TEST_DIR, filename:dirname(?FILE)).
-define(TEST_JSON, filename:join([?TEST_DIR, "test.json"])).
-define(TEST_JSON2, filename:join([?TEST_DIR, "test2.json"])).
-define(TEST_JSON3, filename:join([?TEST_DIR, "test3.json"])).

-define(TEST_GRAPH1, #{
    <<"A">> => [
        #edge{node= <<"N2">>, way_id = 5}
    ],
    <<"B">> => [],
    <<"N1">> => [
        #edge{node= <<"A">>, way_id = 4},
        #edge{node= <<"B">>, way_id = 3}
    ],
    <<"N2">> => [
        #edge{node= <<"V">>, way_id = 1},
        #edge{node= <<"A">>, way_id = 5}
    ],
    <<"V">> => [
        #edge{node= <<"N1">>, way_id = 2}
    ]
}).

-define(TEST_GRAPH1_TRANSP, #{
    <<"A">> => [
        #edge{node = <<"N1">>, way_id = 4},
        #edge{node = <<"N2">>, way_id = 5}
    ],
    <<"B">> => [
        #edge{node = <<"N1">>, way_id = 3}
    ],
    <<"N1">> => [
        #edge{node = <<"V">>, way_id = 2}
    ],
    <<"N2">> => [
        #edge{node = <<"A">>, way_id = 5}
    ],
    <<"V">> => [
        #edge{node = <<"N2">>, way_id = 1}
    ]
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
    <<"A">> => [
        #edge{node = <<"N1">>, way_id = 4}],
    <<"B">> => [],
    <<"N1">> => [
        #edge{node = <<"V">>, way_id = 2},
        #edge{node = <<"B">>, way_id = 3},
        #edge{node = <<"A">>, way_id = 4}
    ],
    <<"N2">> => [
        #edge{node = <<"V">>, way_id = 1},
        #edge{node = <<"A">>, way_id = 5}
    ],
    <<"V">> => [
        #edge{node = <<"N1">>, way_id = 2},
        #edge{node = <<"N2">>, way_id = 1}
    ]
}).

-define(TEST_GRAPH2_TRANSP, #{
    <<"A">> => [
        #edge{node = <<"N2">>, way_id = 5},
        #edge{node = <<"N1">>, way_id = 4}
    ],
    <<"B">> => [
        #edge{node = <<"N1">>, way_id = 3}
    ],
    <<"N1">> => [
        #edge{node = <<"V">>, way_id = 2},
        #edge{node = <<"A">>, way_id = 4}
    ],
    <<"N2">> => [
        #edge{node = <<"V">>, way_id = 1}
    ],
    <<"V">> => [
        #edge{node = <<"N2">>, way_id = 1},
        #edge{node = <<"N1">>, way_id = 2}
    ]
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
    <<"A">> => [
        #edge{node = <<"N1">>, way_id = 4}
    ],
    <<"B">> => [],
    <<"N1">> => [
        #edge{node = <<"V">>, way_id = 2},
        #edge{node = <<"B">>, way_id = 3}
    ],
    <<"N2">> => [
        #edge{node = <<"V">>, way_id = 1},
        #edge{node = <<"A">>, way_id = 5}
    ],
    <<"V">> => [
        #edge{node = <<"N1">>, way_id = 2},
        #edge{node = <<"N2">>, way_id = 1}
    ]
}).

-define(TEST_GRAPH3_TRANSP, #{
    <<"A">> => [
        #edge{node = <<"N2">>, way_id = 5}
    ],
    <<"B">> => [
        #edge{node = <<"N1">>, way_id = 3}
    ],
    <<"N1">> => [
        #edge{node = <<"V">>, way_id = 2},
        #edge{node = <<"A">>, way_id = 4}
    ],
    <<"N2">> => [
        #edge{node = <<"V">>, way_id = 1}
    ],
    <<"V">> => [
        #edge{node = <<"N1">>, way_id = 2},
        #edge{node = <<"N2">>, way_id = 1}
    ]
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
