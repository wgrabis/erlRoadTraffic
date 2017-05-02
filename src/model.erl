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


%% Constants
-define(CAR_SIZE, 5).
-define(LAT_LENGTH, 11.1132).
-define(LON_LENGTH, 7.8847).
-define(DEFAULT_MAXSPEED, "50").
-define(DEAD_END, none_xroad).
-define(DEAD_END_CROSSROAD_SIZE, 1).


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
    {Graph2, _} = filter_not_crossroad_vertices(Graph, TransposeGraph),
    initialize_road_map(#graphData{
      node_data = _Nodes,
      way_data = Ways,
      graph = Graph,
      x_graph = Graph2,
      transposed_graph = TransposeGraph
    }).


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
  Roads = dfs_build_roads(maps:keys(GraphData#graphData.x_graph), GraphData, sets:new()),
  Crossroads = dfs_build_crossroads(maps:keys(GraphData#graphData.x_graph), GraphData, #road_map{roads = Roads}, sets:new()),
  #road_map{
    crossroads = Crossroads,
    roads = Roads
  }.

dfs_build_roads([], _, _)->
  #{};

dfs_build_roads([Head | Tail], GraphData, Visited)->
  case sets:is_element(Head, Visited) of
    true ->
      dfs_build_roads(Tail, GraphData, Visited);
    _ ->
      {Roads, UpdatedVisited} = walk_node_graph(Visited, Head, GraphData),
      ChildRoads = dfs_build_roads(Tail, GraphData, UpdatedVisited),
      maps:merge(Roads, ChildRoads)
  end.

dfs_build_crossroads([], _, _, _)->
  #{};

dfs_build_crossroads([Head | Tail], GraphData, RoadMap, Visited)->
  case sets:is_element(Head, Visited) of
    true ->
      dfs_build_crossroads(Tail, GraphData, RoadMap, Visited);
    _ ->
      {Crossroads, UpdatedVisited} = walk_update_crossroads(Head, GraphData, RoadMap, Visited),
      ChildCrossroads = dfs_build_crossroads(Tail, GraphData, RoadMap, UpdatedVisited),
      maps:merge(Crossroads, ChildCrossroads)
  end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Builds adjacency list for each node.
%%% @end
%%%-------------------------------------------------------------------


walk_node_graph(InputVisited, Node, GraphData) ->
  Visited = sets:add_element(Node, InputVisited),
  {NodeRoads, ChildrenVisited} = build_roads(maps:get(Node, GraphData#graphData.graph), Visited, GraphData, Node),
  {ChildrenRoads, UpdatedVisited} = build_crossroads(maps:get(Node, GraphData#graphData.x_graph), ChildrenVisited, GraphData),
  {maps:merge(NodeRoads, ChildrenRoads), UpdatedVisited}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Function recursively building roads,
%%% for list of neighbouring nodes of a crossroad
%%% @end
%%%-------------------------------------------------------------------

contains_node_helper([], _) ->
  false;

contains_node_helper([#edge{node = Node} | _], Node) ->
  true;

contains_node_helper([_ | Tail], Node) ->
  contains_node_helper(Tail, Node).




build_roads([], CurrVisited, _, _) ->
  {#{}, CurrVisited};
build_roads([NodeEdge| Tail], CurrVisited, GraphData, XNode) ->
  case sets:is_element(NodeEdge#edge.node, CurrVisited) of
    false ->
      {NodeRoad, NodeVisited} = initialize_road(CurrVisited, GraphData, XNode, NodeEdge),
      {TailRoad, TailVisited} = build_roads(Tail, NodeVisited, GraphData, XNode),
      {maps:put(NodeEdge#edge.node, NodeRoad, TailRoad), TailVisited};
    _ ->
      case (maps:is_key(NodeEdge#edge.node, GraphData#graphData.x_graph)
        and not contains_node_helper(maps:get(NodeEdge#edge.node, GraphData#graphData.x_graph, []), XNode)) of
        true ->
          {NodeRoad, NodeVisited} = initialize_road(CurrVisited, GraphData, XNode, NodeEdge),
          {TailRoad, TailVisited} = build_roads(Tail, NodeVisited, GraphData, XNode),
          {maps:put(NodeEdge#edge.node, NodeRoad, TailRoad), TailVisited};
        false ->
          build_roads(Tail, CurrVisited, GraphData, XNode)
      end
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
  {#{}, CurrVisited};
build_crossroads([Node | Tail], CurrVisited, GraphData) ->
  case sets:is_element(Node, CurrVisited) of
    false ->
      {NodeRoads, NodeVisited} = walk_node_graph(CurrVisited, Node, GraphData),
      {TailRoads, TailVisited} = build_crossroads(Tail, NodeVisited, GraphData),
      {maps:merge(NodeRoads, TailRoads),
        TailVisited};
    _ ->
      build_crossroads(Tail, CurrVisited, GraphData)
  end
.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Function used to initialize road
%%% @end
%%%-------------------------------------------------------------------

initialize_road(CurrVisited, GraphData, XNode, StartEdge) ->
  {RisingFractions, UpdatedVisited, EndId, EndEdge, LastNode}
    = initialize_fractions_rising(CurrVisited, GraphData, XNode, StartEdge, 0),

  OppositeEndEdge = get_opposite_edge(LastNode, maps:get(EndEdge#edge.node, GraphData#graphData.graph)),

  case OppositeEndEdge of
    empty ->
      FallingFractions = #{};
    _ ->
      FallingFractions = initialize_fractions_falling(GraphData, EndEdge#edge.node, OppositeEndEdge, XNode, EndId)
  end,

  case maps:is_key(EndEdge#edge.node, GraphData#graphData.x_graph) of
    true ->
      EndXNode = EndEdge#edge.node;
    _ ->
      EndXNode = ?DEAD_END
  end,

  Road = #road{
    id = StartEdge#edge.node,
    begin_crossroad = XNode,
    end_crossroad = EndXNode,
    side_rising = RisingFractions,
    side_falling = FallingFractions,
    no_fractions = EndId + 1
  },

  {Road, UpdatedVisited}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Function used to get opposite edge on connection between
%%% crossroads
%%% @end
%%%-------------------------------------------------------------------

get_opposite_edge(_, [])->
  empty;

get_opposite_edge(TargetNode, [Edge = #edge{node = TargetNode} | _]) ->
  Edge;
get_opposite_edge(TargetNode, [_ | Tail]) ->
  get_opposite_edge(TargetNode, Tail).


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Function to get continuing edge on a road
%%% @end
%%%-------------------------------------------------------------------

get_continuing_edge(_, []) ->
  empty;

get_continuing_edge(PrevNode, [#edge{node=PrevNode} | Tail])->
  get_continuing_edge(PrevNode, Tail);

get_continuing_edge(_, [Head | _]) ->
  Head.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Function used to initalize rising fractions
%%% @end
%%%-------------------------------------------------------------------

initialize_fractions_rising(CurrVisited, GraphData, PrevNode, CurrEdge, CurrId) ->
  case maps:is_key(CurrEdge#edge.node, GraphData#graphData.x_graph) of
    true ->
      {
        #{CurrId => initialize_fraction(GraphData, PrevNode, CurrEdge, CurrId)},
        CurrVisited,
        CurrId,
        CurrEdge,
        PrevNode
      };
    _ ->
      UpdatedVisited = sets:add_element(CurrEdge#edge.node, CurrVisited),
      CurrFraction = initialize_fraction(GraphData, PrevNode, CurrEdge, CurrId),

      case get_continuing_edge(PrevNode, maps:get(CurrEdge#edge.node, GraphData#graphData.graph)) of
        empty ->
          {
            #{CurrId => CurrFraction},
            UpdatedVisited,
            CurrId,
            CurrEdge,
            PrevNode
          };
        NextEdge ->
          {ChildFractions, ChildVisited, MaxId, EndEdge, LastNode} =
            initialize_fractions_rising(UpdatedVisited, GraphData, CurrEdge#edge.node, NextEdge, CurrId + 1),
          {
            maps:put(CurrId, CurrFraction, ChildFractions),
            ChildVisited,
            MaxId,
            EndEdge,
            LastNode
          }
      end
  end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Function used to initialize falling fractions
%%% @end
%%%-------------------------------------------------------------------

initialize_fractions_falling(GraphData, PrevNode, CurrEdge, XCrossEnd, CurrId) ->
  case CurrEdge#edge.node of
    XCrossEnd ->
      #{CurrId => initialize_fraction(GraphData, PrevNode, CurrEdge, CurrId)};
    _ ->
      CurrFraction = initialize_fraction(GraphData, PrevNode, CurrEdge, CurrId),
      OppositeEdge = get_continuing_edge(PrevNode, maps:get(CurrEdge#edge.node, GraphData#graphData.graph)),

      maps:put(CurrId, CurrFraction,
        initialize_fractions_falling(GraphData, CurrEdge#edge.node, OppositeEdge, XCrossEnd, CurrId - 1)
      )
  end.

get_edge_info(GraphData, WayId) ->
  Tags = maps:get(<<"tags">>, maps:get(WayId, GraphData#graphData.way_data, #{})),
  {
    binary_to_integer(maps:get(<<"lanes">>, Tags, <<"1">>)),
    binary_to_integer(maps:get(<<"maxspeed">>, Tags, <<?DEFAULT_MAXSPEED>>))
  }.

count_edge_length(GraphData, BeginNodeId, EndNodeId) ->
  BeginNode = maps:get(EndNodeId, GraphData#graphData.node_data),
  EndNode = maps:get(BeginNodeId, GraphData#graphData.node_data),
  LatDiff = abs(maps:get(<<"lat">>, BeginNode) * 10000 - maps:get(<<"lat">>, EndNode) * 10000),
  LonDiff = abs(maps:get(<<"lon">>, BeginNode) * 10000 - maps:get(<<"lon">>, EndNode) * 10000),
  math:sqrt(math:pow(?LAT_LENGTH * LatDiff, 2) + math:pow(?LON_LENGTH * LonDiff, 2))
  .

initialize_fraction(GraphData, BeginNode, Edge, FractionId) ->

  {NoLanes, MaxSpeed} = get_edge_info(GraphData, Edge#edge.way_id),
  WayLength = count_edge_length(GraphData, BeginNode, Edge#edge.node),
  Lanes = build_lanes(WayLength, NoLanes, 0),

  #road_fraction{
    id = FractionId,
    no_lanes = NoLanes,
    velocity_limit = MaxSpeed,
    lanes =  Lanes
  }.

build_lanes(_, NoLanes, _) when NoLanes == 0 ->
  #{};

build_lanes(Length, NoLanes, CurrId) ->
  Lane = initialize_lane(Length, CurrId),
  maps:put(CurrId, Lane, build_lanes(Length, NoLanes - 1, CurrId + 1)).

initialize_lane(Length, CurrId) ->
  {Cells, NoCells} = build_cells(Length, 0),
  #lane{
    id = CurrId,
    cells = Cells,
    no_cells = NoCells + 1
  }.

build_cells(Length, CurrId) when Length =< ?CAR_SIZE ->
  {#{CurrId => #cell{
    id = CurrId
  }}, CurrId};

build_cells(Length, CurrId) ->
  {ChildCells, MaxCellId} = build_cells(Length - ?CAR_SIZE, CurrId + 1),

  {maps:put(CurrId,
    #cell{id = CurrId},
    ChildCells
  ), MaxCellId}.

walk_update_crossroads(XNode, GraphData, RoadMap, Visited) ->
  UpdatedVisited = sets:add_element(XNode, Visited),
  {ChildCrossroads, ChildVisited} = walk_crossroads(
    maps:get(XNode, GraphData#graphData.x_graph),
    GraphData, RoadMap, UpdatedVisited
  ),
  Crossroad = initialize_crossroad(XNode, RoadMap, GraphData),
  {maps:put(XNode, Crossroad, ChildCrossroads), ChildVisited}.

walk_crossroads([], _, _, Visited) ->
  {#{}, Visited};
walk_crossroads([Node | Tail], GraphData, RoadMap, Visited) ->
  case sets:is_element(Node, Visited) of
    false ->
      {NodeCrossroads, NodeVisited} = walk_update_crossroads(Node, GraphData, RoadMap, Visited),
      {TailCrossroads, TailVisited} = walk_crossroads(Tail, GraphData, RoadMap, NodeVisited),
      {maps:merge(NodeCrossroads, TailCrossroads), TailVisited};
    _ ->
      walk_crossroads(Tail, GraphData, RoadMap, Visited)
  end.


get_adjacent_roads([], _, _, _) ->
  #{};

get_adjacent_roads([Edge | Tail], GraphData, RoadMap, XNode) ->
  Node = Edge#edge.node,
  RoadId = get_road_id(Node, XNode, RoadMap, GraphData, GraphData#graphData.graph),
  case RoadId of
    own_id ->
      maps:put(Node, XNode,
        get_adjacent_roads(Tail, GraphData, RoadMap, XNode));
    _ ->
      maps:put(Node, RoadId,
        get_adjacent_roads(Tail, GraphData, RoadMap, XNode))
  end.



get_road_id(Node, PrevNode, RoadMap, GraphData, CurrGraph) ->
  case maps:is_key(Node, RoadMap#road_map.roads) of
    true ->
      Node;
    _ ->
      case maps:is_key(Node, GraphData#graphData.x_graph) of
        true ->
          own_id;
        _ ->
          ContinuingEdge = get_continuing_edge(PrevNode, maps:get(Node, CurrGraph)),
          get_road_id(
            ContinuingEdge#edge.node,
            Node, RoadMap, GraphData, CurrGraph
          )
      end
  end.


generate_angle_map([], _, _, _) ->
  #{};

generate_angle_map([Edge | Tail], NodeToRoadMap, XNode, GraphData) ->
  Node = Edge#edge.node,
  {X1, Y1} = {
    10000 * maps:get(<<"lat">>, maps:get(Node, GraphData#graphData.node_data)),
    10000 * maps:get(<<"lon">>, maps:get(Node, GraphData#graphData.node_data))
  },
  {X2, Y2} = {
    10000 * maps:get(<<"lat">>, maps:get(XNode, GraphData#graphData.node_data)),
    10000 * maps:get(<<"lon">>, maps:get(XNode, GraphData#graphData.node_data))
  },
  Angle = math:atan2(Y1 - Y2, X1 - X2),
  maps:put(Angle, maps:get(Node, NodeToRoadMap),
    generate_angle_map(Tail, NodeToRoadMap, XNode, GraphData)).



sort_adjacent_roads(XNode, GraphData, AdjacentRoads, EdgeList) ->
  generate_angle_map(EdgeList,
    AdjacentRoads, XNode, GraphData).

build_sorted_roads([], _) ->
  #{};

build_sorted_roads([Road | Tail], Id) ->
  maps:put(Id, Road, build_sorted_roads(Tail, Id + 1)).


sort_angle_roads(AngleRoadMap) ->
  SortedAngles = lists:sort(maps:keys(AngleRoadMap)),
  [ maps:get(K, AngleRoadMap) || K <- SortedAngles].

count_connected_size(RoadId, RoadMap) ->
  Road = maps:get(RoadId, RoadMap#road_map.roads),
  FractionRising = maps:get(0, Road#road.side_rising),
  FractionFalling = maps:get(Road#road.no_fractions - 1, Road#road.side_falling, #road_fraction{
    no_lanes = 0
  }),

  FractionRising#road_fraction.no_lanes
    + FractionFalling#road_fraction.no_lanes.


build_crossroad_size(SortedRoads, RoadMap, NoRoads) ->
  case NoRoads of
    4 ->
      [Road1, Road2, Road3, Road4 | _] = SortedRoads,
      {
        max(count_connected_size(Road1, RoadMap), count_connected_size(Road3, RoadMap)),
        max(count_connected_size(Road2, RoadMap), count_connected_size(Road4, RoadMap))
      };
    3 ->
      [Road1, Road2, Road3 | _] = SortedRoads,
      {
        max(count_connected_size(Road1, RoadMap), count_connected_size(Road3, RoadMap)),
        count_connected_size(Road2, RoadMap)
      };
    1 ->
      [Road1 | _] = SortedRoads,
      {
        count_connected_size(Road1, RoadMap),
        ?DEAD_END_CROSSROAD_SIZE
      };
    0 ->
      {
        ?DEAD_END_CROSSROAD_SIZE,
        ?DEAD_END_CROSSROAD_SIZE
      };
    _ ->
      erlang:error([bad_road_size])
  end.


get_incoming_adjacent_roads([], _, _, _) ->
  #{};

get_incoming_adjacent_roads([Edge | Tail], GraphData, RoadMap, XNode) ->
  Node = Edge#edge.node,
  RoadId = get_road_id(Node, XNode, RoadMap, GraphData, GraphData#graphData.transposed_graph),
  case RoadId of
    own_id ->
      maps:put(Node, XNode,
        get_incoming_adjacent_roads(Tail, GraphData, RoadMap, XNode));
    _ ->
      maps:put(Node, RoadId,
        get_incoming_adjacent_roads(Tail, GraphData, RoadMap, XNode))
  end.


prepare_roads(XNode, RoadMap, GraphData) ->
  AdjacentRoads = get_adjacent_roads(
    maps:get(XNode, GraphData#graphData.graph),
    GraphData, RoadMap, XNode),
  AdjAngleRoadMap = sort_adjacent_roads(XNode, GraphData, AdjacentRoads, maps:get(XNode, GraphData#graphData.graph)),
  AdjSortedRoads = sort_angle_roads(AdjAngleRoadMap),

  TransposedRoads = get_incoming_adjacent_roads(
    maps:get(XNode, GraphData#graphData.transposed_graph), GraphData, RoadMap, XNode),

  UniqueRoads = maps:merge(AdjacentRoads, TransposedRoads),

  AllConnectedEdges = sets:to_list(sets:from_list(
    maps:get(XNode, GraphData#graphData.graph) ++ maps:get(XNode, GraphData#graphData.transposed_graph))),

  AngleRoadMap = sort_adjacent_roads(XNode, GraphData, UniqueRoads, AllConnectedEdges),
  SortedUniqueRoads = sort_angle_roads(AngleRoadMap),
  {AdjSortedRoads, SortedUniqueRoads}.



initialize_crossroad(Node, RoadMap, GraphData) ->
  {SortedRoads, SortedUniqueRoads} = prepare_roads(Node, RoadMap, GraphData),

  {Width, Length} = build_crossroad_size(SortedUniqueRoads, RoadMap, length(SortedUniqueRoads)),
  Cells = build_crossroad_cells(Length, Width),
  #crossroad{
    id = Node,
    cells = Cells,
    roads = build_sorted_roads(SortedRoads, 0),
    length = Length,
    width = Width
  }.

build_no_cells(0, _) ->
  #{};

build_no_cells(NoCells, CurrId) ->
  maps:put(CurrId, #cell{
    id = CurrId
  }, build_no_cells(NoCells - 1, CurrId + 1)).

build_crossroad_cells(Length, Width) ->
  build_no_cells(Width * Length, 0).

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

