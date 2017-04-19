%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(model).
-author({"Jakub Kudzia", "Wojciech Grabis"}).

-include("model.hrl").

%% API
-export([initialize/2]).

-ifdef(TEST).
    %% below functions are exported only for tests
    -export([build_graphs/1, filter_not_crossroad_vertices/2]).
-endif.
-include_lib("eunit/include/eunit.hrl").


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
initialize_road_map(GraphData) ->
  {_, RoadMap} = walk_node_graph(sets:new(), 0, GraphData),
  RoadMap.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Builds adjacency list for each node.
%%% @end
%%%-------------------------------------------------------------------

walk_node_graph(InputVisited, Node, GraphData) ->
  Visited = sets:add_element(Node, InputVisited),
  {ChildrenRoads, ChildrenVisited} = build_roads(maps:get(Node, GraphData#graphData.graph), Visited, GraphData, Node),
  RoadMap = #road_map{
    roads = ChildrenRoads,
    crossroads = #{Node => initialize_crossroad()}
  },
  {ChildrenRoadMap, UpdatedVisited} = build_crossroads(maps:get(Node, GraphData#graphData.x_graph), ChildrenVisited, GraphData),
  UpdatedRoadMap = #road_map{
    roads = maps:merge(RoadMap#road_map.roads, ChildrenRoadMap#road_map.roads),
    crossroads = maps:merge(RoadMap#road_map.crossroads, ChildrenRoadMap#road_map.crossroads)
  },
  {UpdatedRoadMap, UpdatedVisited}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Function recursively building roads,
%%% for list of neighbouring nodes of a crossroad
%%% @end
%%%-------------------------------------------------------------------

build_roads([], CurrVisited, _, _) ->
  {#{}, CurrVisited};
build_roads([NodeEdge| Tail], CurrVisited, GraphData, XNode) ->
  case sets:is_element(NodeEdge#edge.node, CurrVisited) of
    false ->
      {NodeRoad, NodeVisited} = initialize_road(CurrVisited, GraphData, XNode, NodeEdge),
      {TailRoad, TailVisited} = build_roads(Tail, NodeVisited, GraphData, XNode),
      {maps:put(NodeEdge#edge.node, NodeRoad, TailRoad), TailVisited};
    _ ->
      % insert end road info
      build_roads(Tail, CurrVisited, GraphData, XNode)
  end.


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Function recursively building crossroads part of
%%% main dfs function, for list of neighbouring
%%% crossroads
%%% @end
%%%-------------------------------------------------------------------

build_crossroads([], CurrVisited, _)->
  {#road_map{roads = #{}, crossroads = #{}}, CurrVisited};
build_crossroads([Node | Tail], CurrVisited, GraphData) ->
  case sets:is_element(Node, CurrVisited) of
    false ->
      {NodeMap, NodeVisited} = walk_node_graph(CurrVisited, Node, GraphData),
      {TailMap, TailVisited} = build_crossroads(Tail, NodeVisited, GraphData),
      {#road_map{roads = maps:merge(NodeMap#road_map.roads, TailMap#road_map.roads),
          crossroads = maps:merge(NodeMap#road_map.crossroads, TailMap#road_map.crossroads)
        },
        TailVisited};
    _ ->
      build_crossroads(Tail, CurrVisited, GraphData)
  end
.

initialize_road(CurrVisited, GraphData, XNode, StartEdge) ->
  {Fractions, UpdatedVisited} = build_fractions(CurrVisited, GraphData, XNode, StartEdge, 0),
  Road = #road{
    id = StartEdge#edge.node,
    begin_crossroad = XNode,
    fractions = Fractions
  },
  {Road, UpdatedVisited}.

build_fractions(CurrVisited, GraphData, PrevNode, CurrEdge, FractionNumber) ->
  case maps:get(CurrEdge)
  .

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
initialize_crossroad(Node, GraphData) ->
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
    WayId = maps:get(<<"id">>, WayDescription),
    Node1 = hd(Nodes),
    Node2 = lists:last(Nodes),
    Graph2 = update_node(Node1, Node2, Graph, WayId),
    TransposedGraph2 = update_node(Node2, Node1, TransposedGraph, WayId),
    case is_oneway(WayDescription) of
        false ->
            Graph3 = update_node(Node2, Node1, Graph2, WayId),
            TransposedGraph3 = update_node(Node1, Node2, TransposedGraph2, WayId),
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
update_node(Node1, Node2, Graph, WayId) ->
    Edge = #edge{way_id = WayId, node = Node2},
    Graph2 = maps:update_with(Node1, fun(Edges) ->
        [Edge | Edges]
    end, [Edge], Graph),
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
        (V, _Neighbours, {AccIn, TAccIn}) ->
            {to_vertices(V, AccIn), to_vertices(V, TAccIn)}
    end, {Graph, TransposedGraph}, Graph).

to_vertices(V, Graph) ->
    Neighbours = maps:get(V, Graph),
    maps:put(V, [Node || #edge{node=Node} <- Neighbours], Graph).

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
            {to_vertices(V, Graph), to_vertices(V, TransposedGraph)}
    end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
delete_vertex_on_oneway_road(V, #edge{node=FromNode}, To = #edge{node = ToNode}, Graph) ->
    Graph2 = maps:update_with(FromNode, fun(Neighbours) ->
        case lists:member(To, Neighbours) or (FromNode == ToNode) of
            true ->
                lists:filter(fun
                    (#edge{node=Node}) ->
                        Node =/= V;
                    (Node) ->
                        Node =/= V
                end, Neighbours);
            _ ->
                lists:filter(fun
                    (#edge{node=Node}) ->
                        Node =/= V;
                    (Node) ->
                        Node =/= V
                end, [ToNode | Neighbours])
        end
    end, [ToNode], Graph),
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
            {to_vertices(V, Graph), to_vertices(V, TransposedGraph)}
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

