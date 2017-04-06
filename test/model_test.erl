%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Tests of module model.erl
%%% @end
%%%-------------------------------------------------------------------
-module(model_test).
-author("Jakub Kudzia").

-include_lib("eunit/include/eunit.hrl").
-include("model_test.hrl").


model_test_() -> [
    {setup, fun start/0, fun stop/1, fun build_graphs_test_/1},
    {setup, fun start2/0, fun stop/1, fun build_graphs2_test_/1},
    {setup, fun start3/0, fun stop/1, fun build_graphs3_test_/1},
    {setup, fun start/0, fun stop/1, fun filter_not_crossroad_vertices_oneway_test_/1},
    {setup, fun start2/0, fun stop/1, fun filter_not_crossroad_vertices_test_/1},
    {setup, fun start3/0, fun stop/1, fun filter_not_crossroad_vertices2_test_/1}
].


build_graphs_test_({Graph, TransposedGraph}) ->
    [?_assert(graphs_equal(?TEST_GRAPH1, Graph)),
    ?_assert(graphs_equal(?TEST_GRAPH1_TRANSP, TransposedGraph))].

build_graphs2_test_({Graph, TransposedGraph}) ->
    [?_assert(graphs_equal(?TEST_GRAPH2, Graph)),
        ?_assert(graphs_equal(?TEST_GRAPH2_TRANSP, TransposedGraph))].

build_graphs3_test_({Graph, TransposedGraph}) ->
    [?_assert(graphs_equal(?TEST_GRAPH3, Graph)),
        ?_assert(graphs_equal(?TEST_GRAPH3_TRANSP, TransposedGraph))].

filter_not_crossroad_vertices_oneway_test_({Graph, TransposedGraph}) ->
    {FilteredGraph, FilteredTransposedGraph} =
        model:filter_not_crossroad_vertices(Graph, TransposedGraph),
    [?_assert(graphs_equal(?FILTERED_TEST_GRAPH1, FilteredGraph)),
        ?_assert(graphs_equal(?FILTERED_TEST_GRAPH1_TRANSP, FilteredTransposedGraph))].

filter_not_crossroad_vertices_test_({Graph, TransposedGraph}) ->
    {FilteredGraph, FilteredTransposedGraph} =
        model:filter_not_crossroad_vertices(Graph, TransposedGraph),
    [?_assert(graphs_equal(?FILTERED_TEST_GRAPH2, FilteredGraph)),
        ?_assert(graphs_equal(?FILTERED_TEST_GRAPH2_TRANSP, FilteredTransposedGraph))].

filter_not_crossroad_vertices2_test_({Graph, TransposedGraph}) ->
    {FilteredGraph, FilteredTransposedGraph} =
        model:filter_not_crossroad_vertices(Graph, TransposedGraph),
    [?_assert(graphs_equal(?FILTERED_TEST_GRAPH3, FilteredGraph)),
        ?_assert(graphs_equal(?FILTERED_TEST_GRAPH3_TRANSP, FilteredTransposedGraph))].


%%%-------------------------------------------------------------------
%%% Setup and cleanup functions
%%%-------------------------------------------------------------------

start() ->
    MapJson = map_loader:load(?TEST_JSON),
    {_Nodes, Ways} = map_loader:split_to_nodes_and_ways(MapJson),
    model:build_graphs(Ways).

start2() ->
    MapJson = map_loader:load(?TEST_JSON2),
    {_Nodes, Ways} = map_loader:split_to_nodes_and_ways(MapJson),
    model:build_graphs(Ways).

start3() ->
    MapJson = map_loader:load(?TEST_JSON3),
    {_Nodes, Ways} = map_loader:split_to_nodes_and_ways(MapJson),
    model:build_graphs(Ways).

stop(_) -> ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

graphs_equal(Graph1, Graph2) ->
    SortedVertices1 = lists:sort(maps:keys(Graph1)),
    SortedVertices2 = lists:sort(maps:keys(Graph2)),
    ?assertMatch(SortedVertices1, SortedVertices2),
    lists:foreach(fun(V) ->
        V1 = lists:sort(maps:get(V, Graph1)),
        V2 = lists:sort(maps:get(V, Graph2)),
        ?assertMatch(V1, V2)
    end, SortedVertices1),
    true.

