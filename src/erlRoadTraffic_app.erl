%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @doc
%%% erlRoadTraffic public API
%%% @end
%%%-------------------------------------------------------------------
-module(erlRoadTraffic_app).
-author("Jakub Kudzia").

-include("model.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    erlRoadTraffic_sup:start_link(),
    MapJson = map_loader:load(_StartArgs),
    {Nodes, Ways} = map_loader:split_to_nodes_and_ways(MapJson),
    RoadMap = model:initialize(Nodes, Ways),

    RoadId = 1,
    Tours = 10,

    Car = #car{id = 1, velocity = 1},
    Car2 = #car{id = 2, velocity = 2},
    RoadMap1 = model:insert_car(Car, 0, 0, 0, falling, RoadId, RoadMap),
    RoadMap12 = model:insert_car(Car2, 0, 0, 0, rising, RoadId, RoadMap1),

    io:format("#################################RESULT#####################################~n"),
    print(RoadMap12),
    io:format("#################################RESULT#####################################~n"),

    lists:foldl(fun(Tour, RoadMap0) ->
        RoadMap2 = (catch simulation:simulate(RoadMap0, Tour)),
        io:format("#################################RESULT~p####################################~n", [Tour]),
        print(RoadMap2),
        io:format("#################################RESULT#####################################~n"),
        RoadMap2
    end, RoadMap12, lists:seq(1, Tours)).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

print(#road_map{
    roads = Roads,
    crossroads = Xroads
}) ->
    lists:foreach(fun(RoadId) ->
        print_road(maps:get(RoadId, Roads))
    end, lists:sort(maps:keys(Roads))),

    lists:foreach(fun(XRoadId) ->
        print_xroad(maps:get(XRoadId, Xroads))
    end, lists:sort(maps:keys(Xroads))).


print_xroad(#crossroad{id=Id, cells = Cells}) ->
    io:format("Crossroad: ~p~n", [Id]),
    lists:foreach(fun(CellId) ->
        Cell = maps:get(CellId, Cells),
        print_cell(Cell)
    end, lists:sort(maps:keys(Cells))).

print_road(#road{
    id = Id,
    side_rising = SideRising,
    side_falling = SideFalling,
    begin_crossroad = BeginXroad,
    end_crossroad = EndXroad
}) ->
    io:format("Road: ~p: ~p->~p~n", [Id, BeginXroad, EndXroad]),
    io:format("    Side rising~n"),
    print_side(SideRising),
    io:format("    Side falling~n"),
    print_side(SideFalling).


print_side(Side) ->
    lists:foreach(fun(RoadFractionId) ->
        RoadFraction = maps:get(RoadFractionId, Side),
        print_fraction(RoadFraction)
    end, lists:sort(maps:keys(Side))).

print_fraction(#road_fraction{id=Id, lanes = Lanes}) ->
    io:format("        Fraction: ~p~n", [Id]),
    lists:foreach(fun(LaneId) ->
        Lane = maps:get(LaneId, Lanes),
        print_lane(Lane)
    end, lists:sort(maps:keys(Lanes))).

print_lane(#lane{id = Id, cells = Cells}) ->
    io:format("            Lane: ~p~n", [Id]),
    lists:foreach(fun(CellId) ->
        Cell = maps:get(CellId, Cells),
        print_cell(Cell)
    end, lists:sort(maps:keys(Cells))).

print_cell(#cell{car = undefined}) ->
    ok;
print_cell(#cell{id = CellId, car = Car}) ->
    io:format("                Cell: ~p :: ~p~n", [CellId, Car]).