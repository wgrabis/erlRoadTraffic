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
    RoadMap = #road_map{
        roads = Roads,
        crossroads = Crossroads
    } = model:initialize(Nodes, Ways),

    RoadId = 1,
    Road = #road{
%%        side_falling = SideFalling,
        side_rising = SideRising
    } = maps:get(RoadId, Roads),
    Tours = 100,

    %%    io:format("RoadMap: ~p~n", [RoadMap]),
%%    io:format("Roads: ~p~n", [maps:keys(Roads)]),
%%    io:format("Roads: ~p~n", [Roads]),
%%    io:format("Road: ~p~n", [Road]),
%%    io:format("SideRising: ~p~n", [SideRising]),
%%    io:format("SideFalling: ~p~n", [SideFalling]),
%%    io:format("XRoads: ~p~n", [maps:keys(Crossroads)]),
%%    try

    Car = #car{id = 1, velocity = 2},
    RoadMap1 = #road_map{roads = Roads1} = model:insert_car(Car, 0, 0, 0, falling, RoadId, RoadMap),
    Road1 = #road{side_rising = SideRising1} = maps:get(RoadId, Roads1),

    io:format("#################################RESULT#####################################~n"),
    io:format("~p~~n", [SideRising1]),
    io:format("#################################RESULT#####################################~n"),

    lists:foldl(fun(Tour, RoadMap0) ->
        RoadMap2 = #road_map{roads = Roads2} = (catch simulation:simulate(RoadMap0, Tour)),
        Road2 = #road{side_rising = SideRising2} = maps:get(RoadId, Roads2),
        io:format("#################################RESULT#####################################~n"),
        io:format("~p~~n", [SideRising2]),
        io:format("#################################RESULT#####################################~n"),
        RoadMap2
    end, RoadMap1, lists:seq(1, Tours)).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
