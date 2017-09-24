%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @doc
%%% Module containing model building functions
%%% @end
%%%-------------------------------------------------------------------
-module(model).
-author({"Jakub Kudzia", "Wojciech Grabis"}).

-include("model.hrl").
-include("constants.hrl").

%% API
-export([initialize/2, insert_car/7]).

-ifdef(TEST).
    %% below functions are exported only for tests
-export([build_graphs/1, build_crossroad_graphs/2, build_crossroad_graphs_and_simplify/2]).
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
initialize(Nodes, Ways) ->
  {BaseGraph, BaseTransposeGraph} = build_graphs(Ways),
  {Graph, TransposeGraph} = compress_osm_graph(BaseGraph, BaseTransposeGraph),
  {Graph2, TransposeGraph2, Ways2} = update_ways(Graph, TransposeGraph, Ways),
  XGraph = build_crossroad_graphs_and_simplify(Graph2, TransposeGraph2),
  initialize_road_map(#graphData{
    node_data = Nodes,
    way_data = Ways2,
    graph = Graph2,
    x_graph = XGraph,
    transposed_graph = TransposeGraph2
  }).

insert_car(Car, CellId, LaneId, FractionId, Side, RoadId,
    RoadMap = #road_map{roads=Roads}
) ->
    Road = maps:get(RoadId, Roads),
    Road2 = insert_car_to_road(Car, CellId, LaneId, FractionId, Side, Road),
    RoadMap#road_map{roads = Roads#{RoadId => Road2}}.

insert_car_to_road(Car, CellId, LaneId, FractionId, falling,
    Road = #road{side_falling=SideFalling}
) ->
    Fraction = maps:get(FractionId, SideFalling),
    Fraction2 = insert_car_to_fraction(Car, CellId, LaneId, Fraction),
    Road#road{side_falling = SideFalling#{FractionId => Fraction2}};
insert_car_to_road(Car, CellId, LaneId, FractionId, rising,
    Road = #road{side_rising=SideRising}
) ->
    Fraction = maps:get(FractionId, SideRising),
    Fraction2 = insert_car_to_fraction(Car, CellId, LaneId, Fraction),
    Road#road{side_rising = SideRising#{FractionId => Fraction2}}.

insert_car_to_fraction(Car, CellId, LaneId, Fraction = #road_fraction{lanes=Lanes}) ->
    Lane = maps:get(LaneId, Lanes),
    Lane2 = insert_car_to_lane(Car, CellId, Lane),
    Fraction#road_fraction{lanes = Lanes#{LaneId => Lane2}}.

insert_car_to_lane(Car, CellId, Lane = #lane{cells = Cells}) ->
    Cell = maps:get(CellId, Cells),
    Cell2 = insert_car_to_cell(Car, Cell),
    Lane#lane{cells = Cells#{CellId => Cell2}}.

insert_car_to_cell(Car, Cell) ->
    Cell#cell{car=Car}.




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
      {maps:put(NodeEdge#edge.way_id, NodeRoad, TailRoad), TailVisited};
    _ ->
      case (maps:is_key(NodeEdge#edge.node, GraphData#graphData.x_graph)
        and not contains_node_helper(maps:get(NodeEdge#edge.node, GraphData#graphData.x_graph, []), XNode)) of
        true ->
          {NodeRoad, NodeVisited} = initialize_road(CurrVisited, GraphData, XNode, NodeEdge),
          {TailRoad, TailVisited} = build_roads(Tail, NodeVisited, GraphData, XNode),
          {maps:put(NodeEdge#edge.way_id, NodeRoad, TailRoad), TailVisited};
        false ->
          build_roads(Tail, CurrVisited, GraphData, XNode)
      end
  end.


%%-------------------------------------------------------------------
%% @private
%% @doc
%% WRITEME
%% @end
%%-------------------------------------------------------------------
update_ways(CompressedGraph, TCompressedGraph, Ways) ->
  {CompressedGraph1, CompressedGraphUnchanged1, TCompressedGraph1, TCompressedGraphUnchanged1, ChangedWays, UnchangedWays} =
    filter_unchanged_ways(CompressedGraph, TCompressedGraph, Ways),

  update_ways(CompressedGraph1, CompressedGraphUnchanged1, TCompressedGraph1, TCompressedGraphUnchanged1, ChangedWays, UnchangedWays).

filter_unchanged_ways(Graph, TGraph, Ways) ->
  maps:fold(fun(WayId, Way,
      {Graph0, GraphUnchangedWays0, TGraph0, TGraphUnchangedWays0, Ways0, UnchangedWays}
  ) ->
    Nodes = maps:get(<<"nodes">>, Way),
    From = hd(Nodes),
    To = lists:last(Nodes),
    case is_oneway(Way) of
      true ->
        case edge_in_graph({From, To}, WayId, Graph0) of
          true ->
            Graph1 = delete_edge({From, To}, WayId, Graph0),
            TGraph1 = delete_edge({To, From}, WayId, TGraph0),
            Ways1 = maps:remove(WayId, Ways0),
            GraphUnchangedWays1 = update_node(From, To, GraphUnchangedWays0, WayId),
            TGraphUnchangedWays1 = update_node(To, From, TGraphUnchangedWays0, WayId),
            UnchangedWays1 = UnchangedWays#{WayId => Way},
            {Graph1, GraphUnchangedWays1, TGraph1, TGraphUnchangedWays1, Ways1, UnchangedWays1};
          false ->
            {Graph0, GraphUnchangedWays0, TGraph0, TGraphUnchangedWays0, Ways0, UnchangedWays}
        end;
      false ->
        case edge_in_graph({From, To}, WayId, Graph0) of
          true ->
            Graph1 = delete_edge({From, To}, WayId, Graph0),
            Graph2 = delete_edge({To, From}, WayId, Graph1),
            GraphUnchangedWays1 = update_node(From, To, GraphUnchangedWays0, WayId),
            GraphUnchangedWays2 = update_node(To, From, GraphUnchangedWays1, WayId),
            TGraph1 = delete_edge({To, From}, WayId, TGraph0),
            TGraph2 = delete_edge({From, To}, WayId, TGraph1),
            TGraphUnchangedWays1 = update_node(To, From, TGraphUnchangedWays0, WayId),
            TGraphUnchangedWays2 = update_node(From, To, TGraphUnchangedWays1, WayId),
            Ways1 = maps:remove(WayId, Ways0),
            UnchangedWays1 = UnchangedWays#{WayId => Way},
            {Graph2, GraphUnchangedWays2, TGraph2, TGraphUnchangedWays2, Ways1, UnchangedWays1};
          false ->
            {Graph0, GraphUnchangedWays0, TGraph0, TGraphUnchangedWays0, Ways0, UnchangedWays}
      end
    end
  end, {Graph, #{}, TGraph, #{}, Ways, #{}}, Ways).

edge_in_graph({From, To}, WayId, Graph) ->
  case maps:get(From, Graph, undefined) of
    undefined ->
      false;
    Nodes ->
      lists:member(#edge{way_id = WayId, node = To}, Nodes)
  end.

delete_edge({From, To}, WayId, Graph) ->
  Edge = #edge{way_id = WayId, node = To},
  maps:update_with(From, fun(Nodes) ->
    Nodes -- [Edge]
  end, Graph).

update_ways(Graph, GraphUnchangedWays, TGraph, TGraphUnchangedWays, ChangedWays, UnchangedWays) ->
  {NewGraph, NewTGraph, NewWays} = maps:fold(fun(From, Nodes, AccIn) ->
    lists:foldl(fun(#edge{way_id = WayId, node = To}, {Graph0, TGraph0, ChangedWays0}) ->
        Way = maps:get(WayId, ChangedWays),
        Graph1 = delete_edge({From, To}, WayId, Graph0),
        TGraph1 = delete_edge({To, From}, WayId, TGraph0),
        NewWayId = new_way_id(From, To, WayId),
        Graph2 = update_node(From, To, Graph1, NewWayId),
        TGraph2 = update_node(To, From, TGraph1, NewWayId),
        ChangedWays1 = ChangedWays0#{NewWayId => Way#{
            <<"id">> => NewWayId,
            <<"nodes">> => [From, To]
        }},
        {Graph2, TGraph2, ChangedWays1}
    end, AccIn, Nodes)
  end, {Graph, TGraph, ChangedWays}, Graph),
    Ways1 = maps:merge(NewWays, UnchangedWays),
    WaysToDelete = maps:keys(ChangedWays),
    Ways2 = maps:filter(fun(WayId, _Way) ->
        not lists:member(WayId, WaysToDelete)
    end, Ways1),
  {
    merge(NewGraph, GraphUnchangedWays),
    merge(NewTGraph, TGraphUnchangedWays),
    Ways2
  }.

merge(Graph1, Graph2) ->
    maps:fold(fun(From, Nodes, AccIn) ->
        Nodes2 = maps:get(From, Graph2, []),
        AccIn#{From => Nodes ++ Nodes2}
    end, #{}, Graph1).

new_way_id(From, To, WayId) when From =< To ->
  Args = [ensure_binary(From), ensure_binary(To), ensure_binary(WayId)],
  crypto:hash(md5, Args);
new_way_id(From, To, WayId) ->
  new_way_id(To, From, WayId).


ensure_binary(Arg) when is_binary(Arg) -> Arg;
ensure_binary(Arg) when is_list(Arg) -> list_to_binary(Arg);
ensure_binary(Arg) when is_integer(Arg) -> integer_to_binary(Arg).


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
    id = StartEdge#edge.way_id,
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


check_orientation(Way, PrevNode, CurrNode) ->
  Nodes = maps:get(<<"nodes">>, Way, []),
  [NextNode | _] = lists:dropwhile(
    fun (Elem) ->
      case Elem of
        PrevNode ->
          true;
        _ ->
          false
      end
    end, Nodes),
  case NextNode of
    CurrNode ->
      <<"forward">>;
    _ ->
      <<"backward">>
  end.



build_turn_info(BinaryTurnInfo) when BinaryTurnInfo == none->
  none;

build_turn_info(BinaryTurnInfo)->
  TurnInfoList = re:split(BinaryTurnInfo, "\\|"),
  {OutTurnInfo, _} = lists:foldl(fun
                  (Record, {CurrMap, Index}) ->
                    case Record of
                      <<"">> ->
                        {CurrMap, Index + 1};
                      _ ->
                        Turns = lists:foldl(fun
                                              (CurrInfo, CurrList) ->
                                                case CurrInfo of
                                                  <<"left">> ->
                                                    Turn = ?LEFT_RULE;
                                                  <<"through">> ->
                                                    Turn = ?STRAIGHT_RULE;
                                                  <<"right">> ->
                                                    Turn = ?RIGHT_RULE;
                                                  <<"merge_to_left">> ->
                                                    Turn = ?MERGE_LEFT;
                                                  <<"merge_to_right">> ->
                                                    Turn = ?MERGE_RIGHT
                                                end,
                                                CurrList ++ [Turn]
                                            end, [],re:split(Record, ";")),
                        {maps:put(Index, Turns, CurrMap), Index + 1}
                    end
                end, {#{}, 0}, TurnInfoList),
  OutTurnInfo.


get_edge_info(GraphData, WayId, PrevNode, CurrNode) ->
  Way = maps:get(WayId, GraphData#graphData.way_data),
  Orientation = check_orientation(Way, PrevNode, CurrNode),
  Tags = maps:get(<<"tags">>, Way, #{}),
  Lanes = case maps:get(<<"lanes:", Orientation/binary>>, Tags, none) of
    none ->
      case maps:get(<<"oneway">>, Tags, <<"no">>) of
        <<"no">> ->
          binary_to_integer(maps:get(<<"lanes">>, Tags, <<"2">>)) div 2;
        <<"yes">> ->
          binary_to_integer(maps:get(<<"lanes">>, Tags, <<"1">>))
      end;
    Value ->
      binary_to_integer(Value)
  end,
  case maps:get(<<"oneway">>, Tags, <<"no">>) of
    <<"no">> ->
      TurnInfo = build_turn_info(maps:get(<<"turn:lanes:", Orientation/binary>>, Tags, none));
    <<"yes">> ->
      TurnInfo = build_turn_info(maps:get(<<"turn:lanes:">>, Tags, none))
  end,
  {
    Lanes,
    TurnInfo,
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

  {NoLanes, TurnInfo, MaxSpeed} = get_edge_info(GraphData, Edge#edge.way_id,BeginNode, Edge#edge.node),

  WayLength = count_edge_length(GraphData, BeginNode, Edge#edge.node),
  Lanes = build_lanes(WayLength, NoLanes, 0),

  case TurnInfo of
      none ->
          EndTurnInfo = #{};
      Val ->
          EndTurnInfo = Val
  end,


  #road_fraction{
    id = FractionId,
    no_lanes = NoLanes,
    velocity_limit = MaxSpeed,
    lanes =  Lanes,
    special_rules = EndTurnInfo
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
  RoadId = get_road_id(Edge, XNode, RoadMap, GraphData, GraphData#graphData.graph),
  case RoadId of
    none_id ->
      erlang:error("No id found");
    _ ->
      maps:put(Node, RoadId,
        get_adjacent_roads(Tail, GraphData, RoadMap, XNode))
  end.


%%edge - syf przeleciec ponaprawiac
get_road_id(#edge{node = Node, way_id = EdgeId}, PrevNode, RoadMap, GraphData, CurrGraph) ->
  case maps:is_key(EdgeId, RoadMap#road_map.roads) of
    true ->
      EdgeId;
    _ ->
      case maps:is_key(Node, GraphData#graphData.x_graph) of
        true ->
          none_id;
        _ ->
          ContinuingEdge = get_continuing_edge(PrevNode, maps:get(Node, CurrGraph)),
          get_road_id(
            ContinuingEdge,
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
  RoadId = get_road_id(Edge, XNode, RoadMap, GraphData, GraphData#graphData.transposed_graph),
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
  [_ | FoldNodes] = Nodes,
  {_, ResultGraph, ResultTransposedGraph} = lists:foldl(
    fun (NodeCurr, {PrevNode, CurrGraph, CurrTransposedGraph}) ->
      Graph2 = update_node(PrevNode, NodeCurr, CurrGraph, WayId),
      TransposedGraph2 = update_node(NodeCurr, PrevNode, CurrTransposedGraph, WayId),
      case is_oneway(WayDescription) of
        false ->
          Graph3 = update_node(NodeCurr, PrevNode, Graph2, WayId),
          TransposedGraph3 = update_node(PrevNode, NodeCurr, TransposedGraph2, WayId),
          {NodeCurr, Graph3, TransposedGraph3};
        _ ->
          {NodeCurr, Graph2, TransposedGraph2}
      end
    end, {Node1, Graph, TransposedGraph}, FoldNodes
  ),
  {ResultGraph, ResultTransposedGraph}.


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
build_crossroad_graphs_and_simplify(Graph, TransposedGraph) ->
  case build_crossroad_graphs(Graph, TransposedGraph) of
    {Graph, TransposedGraph} -> simplify_crossroad_graph(Graph);
    {Graph2, TransposedGraph2} -> build_crossroad_graphs_and_simplify(Graph2, TransposedGraph2)
  end.

build_crossroad_graphs(Graph, TransposedGraph) ->
    {XGraph, TransposedXGraph} = maps:fold(fun (V, _, {CurrGraph, CurrTransposedGraph}) ->
        filter_vertices(V, CurrTransposedGraph, CurrGraph)
    end, {Graph, TransposedGraph}, Graph),
  {XGraph, TransposedXGraph}.


simplify_crossroad_graph(Graph) ->
  maps:fold(
    fun (V, AdjList, CurrGraph) ->
      NewAdjList = lists:foldl(
        fun(#edge{node = Node}, CurrList) ->
          CurrList ++ [Node]
        end, [], AdjList
      ),
      maps:put(V, lists:usort(NewAdjList), CurrGraph)
    end, #{}, Graph
  ).

%% usuwa zbedne wierzcholki A -> B -> C => A -> C
filter_vertices(V, CurrTransposedGraph, CurrGraph) ->
  AdjList = maps:get(V, CurrGraph),
  TransposedAdjList = maps:get(V, CurrTransposedGraph),
  AdjSet = lists:foldl(fun (#edge{node = Node}, CurrSet) ->
                sets:add_element(Node, CurrSet)
              end, sets:new(), AdjList ++ TransposedAdjList
  ),
  case sets:size(AdjSet) of
    2 ->
      [Node1, Node2] = sets:to_list(AdjSet),
      reconnect_vertices(Node1, Node2, V, CurrGraph, CurrTransposedGraph);
    _ ->
      {CurrGraph, CurrTransposedGraph}
  end.



reconnect_vertices(Node1, Node2, RemovedNode, CurrGraph, CurrTransposedGraph) ->
  {Upd1Graph, Upd1TrGraph} = update_both_graphs(Node1, RemovedNode, Node2, CurrGraph, CurrTransposedGraph),
  {Upd2Graph, Upd2TrGraph} = update_both_graphs(Node2, RemovedNode, Node1, Upd1Graph, Upd1TrGraph),
  {maps:remove(RemovedNode, Upd2Graph), maps:remove(RemovedNode, Upd2TrGraph)}.


update_both_graphs(NodeX, RemovedNode, NewNode, CurrGraph, CurrTransposedGraph) ->
  RegularAdjList = get_updated_adj(NodeX, RemovedNode, NewNode, CurrGraph),
  TrnAdjList = get_updated_adj(NodeX, RemovedNode, NewNode, CurrTransposedGraph),
  {maps:update(NodeX, RegularAdjList, CurrGraph),
    maps:update(NodeX, TrnAdjList, CurrTransposedGraph)}.

get_updated_adj(NodeX, RemovedNode, NewNode, CurrGraph) ->
  BaseAdjList = maps:get(NodeX, CurrGraph),
  NewAdjList = lists:foldl(
    fun (#edge{ node = Node, way_id = WayId}, CurrList) ->
      case Node of
        RemovedNode ->
          CurrList ++ [#edge{node = NewNode, way_id = WayId}];
        NodeX ->
          CurrList;
        RegNode ->
          CurrList ++ [#edge{node = RegNode, way_id = WayId}]
      end
    end, [], BaseAdjList
  ),
  NewAdjList.

% usuwa wierzchołki, ktore znajduja sie na srodku drogi i nie sa skrzyzowaniami
% maja dwoch sasiadow, polączonych droga o tym samym wayid
compress_osm_graph(Graph, TransposedGraph) ->
  maps:fold(
    fun (V, _, {CurrGraph, CurrTransGraph}) ->
      check_vertex_redundancy(V, CurrGraph, CurrTransGraph)
    end, {Graph, TransposedGraph}, Graph
  ).

compare_edge_sets([#edge{node = Node1, way_id = WayId}], [#edge{node = Node2, way_id = WayId}]) when Node1 =/= Node2->
  {true, Node1, Node2};

compare_edge_sets([#edge{node = Node1, way_id = WayId}, #edge{node = Node2, way_id = WayId}], [#edge{node = Node2, way_id = WayId}, #edge{node = Node1, way_id = WayId}]) ->
  {true, Node1, Node2};

compare_edge_sets([#edge{node = Node1, way_id = WayId}, #edge{node = Node2, way_id = WayId}], [#edge{node = Node1, way_id = WayId}, #edge{node = Node2, way_id = WayId}]) ->
  {true, Node1, Node2};

compare_edge_sets(_, _) ->
  {false, none, none}.

check_vertex_redundancy(V, Graph, TransGraph) ->
  case compare_edge_sets(
    maps:get(V, Graph),
    maps:get(V, TransGraph)
  ) of
    {true, Node1, Node2} ->
      reconnect_vertices(Node1, Node2, V, Graph, TransGraph);
    {false, _, _} ->
      {Graph, TransGraph}
  end.

%%update_repeated_edges(Graph, TransGraph, XGraph) ->
%%  maps:fold(
%%    fun (V, _, {EdgeSet, }) ->
%%      AdjList = maps:get(V, Graph),
%%      lists:foldl(
%%
%%      )
%%    end, {sets:new()}, XGraph
%%  ).

%%remove_double_edges(AdjList, Graph, TransGraph, Id, EdgeSet) ->
%%  lists:foldl(
%%    fun(#edge{node = Node, way_id = WayId}, {CurrGraph, CurrTransGraph, CurrId, CurrEdgeSet})->
%%      case sets:is_element(WayId, CurrEdgeSet) of
%%        true->
%%          ;
%%        false ->
%%
%%      end
%%    end, {Graph, TransGraph, Id, EdgeSet}, AdjList
%%  )
%%
%%update_edge_id(Node1, Node2, NewEdgeId, OldEdgeId, Graph, TransGraph)->
%%
%%
%%  {}.