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
    Road = #road{
        side_falling = SideFalling,
        side_rising = SideRising
    } = maps:get(119918895, Roads),
%%    io:format("RoadMap: ~p~n", [RoadMap]),
%%    io:format("Roads: ~p~n", [maps:keys(Roads)]),
%%    io:format("Roads: ~p~n", [Roads]),
%%    io:format("Road: ~p~n", [Road]),
%%    io:format("SideRising: ~p~n", [SideRising]),
%%    io:format("SideFalling: ~p~n", [SideFalling]),
%%    io:format("XRoads: ~p~n", [maps:keys(Crossroads)]),
%%    try
    RoadId = 119918895,
    Car = #car{id = 1, velocity = 2},
    RoadMap2 = model:insert_car(Car, 0, 0, 0, rising, RoadId, RoadMap),
    io:format("RoadMap2: ~p~n", [RoadMap2]),
    R = (catch simulation:simulate(RoadMap2)),
%%    io:format("#################################Error#####################################~n"),
    io:format("#################################RESULT#####################################~n"),
    io:format("#################################RESULT#####################################~n"),
    io:format("#################################RESULT#####################################~n"),
    io:format("~p~n", [R]).
%%    catch
%%        E1:E2 ->
%%            io:format("~p:~p", [E1, E2])
%%    end.


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
