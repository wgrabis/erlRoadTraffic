%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(model).
-author("Jakub Kudzia").

-include("model.hrl").

%% API
-export([initialize/2]).

-ifdef(TEST).
    %% below functions are exported only for tests
    -export([build_graphs/1, filter_not_crossroad_vertices/2]).
-endif.
-include_lib("eunit/include/eunit.hrl").
-record(graphData, {
  graph :: #{id() => []},
  x_graph :: #{id() => []},
  node_data :: #{id() => []},
  way_data :: #{id() => []}}
}).


%%%-------------------------------------------------------------------
%%% API functions
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
initialize(_Nodes, Ways) ->
    {Graph, TransposeGraph} = build_graphs(Ways),
    {Graph2, TransposeGraph2} = filter_not_crossroad_vertices(Graph, TransposeGraph),
    io:format("~p~n~p", [Graph, Graph2]).


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Builds adjacency list for each node.
%%% @end
%%%-------------------------------------------------------------------
initialize_road_map(Nodes, Graph, XGraph) ->
    #road_map{
        roads = initialize_roads(Nodes, Ways),
        crossroads = initialize_crossroads(Nodes, Ways)
    }.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Builds adjacency list for each node.
%%% @end
%%%-------------------------------------------------------------------

walk_node_graph(Visited, Node, Nodes, Graph, XGraph) ->
  UpdatedVisited = sets:add_element(Node, Visited),
  RoadMap = #road_map{
    roads = #{},
    crossroads = #{Node => initialize_crossroad()}
  },
  case maps:get(Node, Graph) of
    Neighbours ->
      UpdatedRoadMap = build_roads(Neighbours, Visited, Nodes, Graph, XGraph, Node, RoadMap)
  end,
  case maps:get(Node, XGraph) of
    Neighbours ->
      lists:foldl(
      (fun(Elem, {CurrRoadMap, CurrVisited}) ->
        case sets:is_element(Elem, CurrVisited) of
          false ->
            {ChildVisited, ChildMap} = walk_node_graph(CurrVisited, Elem, Nodes, Graph, XGraph),
            {maps:merge(CurrRoadMap, ChildMap), sets:union(ChildVisited, CurrVisited)};
          _ ->
            {CurrRoadMap, CurrVisited}
        end
      end),
      {RoadMap, UpdatedVisited},
      Neighbours
      )
  end,
  {UpdatedVisited, UpdatedRoadMap}.

% - zwrocic visited wierzcholki


build_roads([Node| Tail], CurrVisited, Nodes, Graph, XGraph, XNode, RoadMap) ->
  case sets:is_element(Node, CurrVisited) of
    false ->
      UpdatedVisited = sets:add_element(Node, CurrVisited),
      {NodeRoad, NodeVisited} = initialize_road(UpdatedVisited, Graph, XGraph, Nodes, XNode, Node),
      {TailRoad, TailVisited} = build_roads(Tail, NodeVisited, Nodes, Graph, XGraph, XNode, NodeRoad),
      {maps:put(Node, TailRoad, RoadMap), sets:union(CurrVisited, TailVisited)};
    _ ->
      build_roads(Tail, CurrVisited, Nodes, Graph, XGraph, XNode, RoadMap)
  end.


build_child_crossroads([Node | Tail], CurrVisited, Nodes, Graph, XGraph, RoadMap) ->


initialize_road(CurrVisited, Graph, XGraph, Nodes, XNode, StartNode) ->
  Visited = sets:add_element(StartNode, sets:new()),
  Road = 0,
  {Road, Visited}.

%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
initialize_road(Nodes, WayDescription, Ways)  ->
    erlang:error(not_implemented).



%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Builds adjacency list for each node.
%%% @end
%%%-------------------------------------------------------------------
initialize_crossroad(Node, Graph, XGraph, Ways) ->
    erlang:error(not_implemented).

%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
initialize_fraction(Nodes, WayDescription, Ways) ->
    erlang:error(not_implemented).

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Builds directed graph and its transposition. Vertices are nodes
%%% and edges are ways from OSM JSON.
%%% NOTE !!! Vertex in this graph doesn't have to be a crossroad
%%% (nodes in the middle of the roads).
%%% @end
%%%-------------------------------------------------------------------
build_graphs(Ways) ->
    maps:fold(fun(_WayId, WayDescription, AccIn) ->
        add_way(WayDescription, AccIn)
    end, {#{}, #{}}, Ways).


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Adds end Nodes of given Way to Graph and its transposition
%%% TransposedGraph.
%%% @end
%%%-------------------------------------------------------------------
add_way(WayDescription, {Graph, TransposedGraph}) ->
    Nodes = maps:get(<<"nodes">>, WayDescription),
    Node1 = hd(Nodes),
    Node2 = lists:last(Nodes),
    Graph2 = update_node(Node1, Node2, Graph),
    TransposedGraph2 = update_node(Node2, Node1, TransposedGraph),
    case is_oneway(WayDescription) of
        false ->
            Graph3 = update_node(Node2, Node1, Graph2),
            TransposedGraph3 = update_node(Node1, Node2, TransposedGraph2),
            {Graph3, TransposedGraph3};
        _ ->
            {Graph2, TransposedGraph2}
    end.


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Updates entry in adjacency matrix for given Node1.
%%% Node2 is added to list of Node1's neighbours list in Graph.
%%% @end
%%%-------------------------------------------------------------------
update_node(Node1, Node2, Graph) ->
    Graph2 = maps:update_with(Node1, fun(Nodes) ->
        [Node2 | Nodes]
    end, [Node2], Graph),
    case maps:is_key(Node2, Graph2) of
        false ->
            Graph2#{Node2 => []};
        _ -> Graph2
    end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Checks if given way is oneway.
%%% @end
%%%-------------------------------------------------------------------
is_oneway(#{<<"tags">> := #{<<"oneway">> := <<"yes">>}}) ->
    true;
is_oneway(_) ->
    false.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
filter_not_crossroad_vertices(Graph, TransposedGraph) ->
    maps:fold(fun
        (V, [N1], {AccIn, TAccIn}) ->
            maybe_delete_vertex_on_oneway_road(V, N1, AccIn, TAccIn);
        (V, [N1, N2], {AccIn, TAccIn}) ->
            maybe_delete_vertex_on_twoway_road(V, N1, N2, AccIn, TAccIn);
        (_V, _Neighbours, AccIn) ->
            AccIn
    end, {Graph, TransposedGraph}, Graph).

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
maybe_delete_vertex_on_oneway_road(V, To, Graph, TransposedGraph) ->
    case maps:get(V, TransposedGraph) of
        [From] ->
            {delete_vertex_on_oneway_road(V, From, To, Graph),
                delete_vertex_on_oneway_road(V, To, From, TransposedGraph)};
        _ ->
            {Graph, TransposedGraph}
    end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
delete_vertex_on_oneway_road(V, From, To, Graph) ->
    Graph2 = maps:update_with(From, fun(Neighbours) ->
        case lists:member(To, Neighbours) or (From == To) of
            true ->
                Neighbours -- [V];
            _ ->
                [To | Neighbours] -- [V]
        end
    end, [To], Graph),
    maps:remove(V, Graph2).


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
maybe_delete_vertex_on_twoway_road(V, To1, To2, Graph, TransposedGraph) ->
    case maps:get(V, TransposedGraph) of
        Neighbours when
            Neighbours == [To1, To2];
            Neighbours == [To2, To1]
            ->
                delete_vertex_on_twoway_road(V, To1, To2, Graph, TransposedGraph);
        _ ->
            {Graph, TransposedGraph}
    end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
delete_vertex_on_twoway_road(V, To1, To2, Graph, TransposedGraph) ->
    G2 = delete_vertex_on_oneway_road(V, To1, To2, Graph),
    T2 = delete_vertex_on_oneway_road(V, To2, To1, TransposedGraph),
    G3 = delete_vertex_on_oneway_road(V, To2, To1, G2),
    T3 = delete_vertex_on_oneway_road(V, To1, To2, T2),
    {G3, T3}.

