%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(simulation).
-author("Jakub Kudzia").

-include("model.hrl").
-include("constants.hrl").

%% API
-export([]).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
simulate(#road_map{roads=Roads, crossroads=Crossroads}) ->

    {UpdatedRoads, UpdatedCrossroads} = progress_cars_on_roads(Roads, Crossroads),
%%    {UpdatedRoads2, UpdatedCrossroads2} = progress_cars_on_xroads(UpdatedRoads, UpdatedCrossroads),
    #road_map{roads= UpdatedRoads, crossroads = UpdatedCrossroads}.

%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_xroads(Roads, Crossroads) ->
    erlang:error(not_implemented).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_roads(Roads0, Crossroads0) ->
    maps:fold(fun(RoadId, Road, {Roads, Crossroads}) ->
        {UpdatedRoad, UpdatedCrossroads} = progress_cars_on_road(Road, Crossroads),
        {Roads#{RoadId => UpdatedRoad}, UpdatedCrossroads}
    end, {Roads0, Crossroads0} , Roads0).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_road(Road = #road{
    side_falling = SideFalling0,
    side_rising = SideRising0,
    begin_crossroad = BeginCrossroad0,
    end_crossroad = EndCrossroad0,
    no_fractions = NoFractions
}, Crossroads0) ->

    {SideFalling, BeginCrossroad, EndCrossroad, UpdatedCrossroads} =
        progress_cars_on_side(SideFalling0, BeginCrossroad0, EndCrossroad0, Crossroads0),
    {SideRising, BeginCrossroad2, EndCrossroad2, UpdatedCrossroads2} =
        progress_cars_on_side(SideRising0, BeginCrossroad, EndCrossroad, UpdatedCrossroads),
    {Road#road{
        side_falling = SideFalling,
        side_rising = SideRising,
        begin_crossroad = BeginCrossroad2,
        end_crossroad = EndCrossroad2
    }, UpdatedCrossroads2}.


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_side(RoadFractions0, BeginCrossroad0, EndCrossroad0, Crossroads0) ->
    Keys = maps:keys(RoadFractions0),
    lists:foldr(fun(FractionId, {RoadFractions, BeginCrossroad, EndCrossroad, Crossroads}) ->
        {UpdatedFractions, UpdatedBeginCrossroad, UpdatedEndCrossroad, UpdatedCrossroads} =
            progress_cars_on_road_fraction(FractionId, RoadFractions, BeginCrossroad, EndCrossroad, Crossroads),
        {
            UpdatedFractions,
            UpdatedBeginCrossroad,
            UpdatedEndCrossroad,
            UpdatedCrossroads
        }
    end, {RoadFractions0, BeginCrossroad0, EndCrossroad0, Crossroads0}, Keys).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_road_fraction(FractionId, RoadFractions, BeginCrossroad, EndCrossroad, Crossroads) ->
    RoadFraction = #road_fraction{
        lanes = Lanes
    }= maps:get(FractionId, RoadFractions),

    {UpdatedRoadFractions, UpdatedBeginCrossroad, UpdatedEndCrossroad, UpdatedCrossroads} =
        progress_cars_on_lanes(Lanes, RoadFractions, BeginCrossroad, EndCrossroad, Crossroads),
    {
        UpdatedRoadFractions,
        UpdatedBeginCrossroad,
        UpdatedEndCrossroad,
        UpdatedCrossroads
    }.


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_lanes(Lanes0, RoadFractions, BeginCrossroad0, EndCrossroad0, Crossroads0) ->
    maps:fold(fun(LaneId, Lane, {Lanes, BeginCrossroad, EndCrossroad, Crossroads}) ->
        {UpdatedLane, UpdatedBeginCrossroad, UpdatedEndCrossroad, UpdatedCrossroads} =
            progress_cars_on_lane(Lane, BeginCrossroad, EndCrossroad, Crossroads),
        {Lanes#{LaneId => UpdatedLane}, UpdatedBeginCrossroad, UpdatedEndCrossroad, UpdatedCrossroads}
    end, {Lanes0, BeginCrossroad0, EndCrossroad0, Crossroads0}, Lanes0).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_lane(Lane = #lane{cells=Cells0, no_cells = NoCells},
    BeginCrossroad0, EndCrossroad0, Crossroads0
) ->
    {UpdatedCells, UpdatedBeginCrossroad, UpdatedEndCrossroad, UpdatedCrossroads} =
        progress_cars_on_cells(Cells0, NoCells, BeginCrossroad0, EndCrossroad0, Crossroads0),
    {
        Lane#lane{cells = UpdatedCells},
        UpdatedBeginCrossroad,
        UpdatedEndCrossroad,
        UpdatedCrossroads
    }.


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_cells(Cells0, NoCells, BeginCrossroad0, EndCrossroad0, CrossRoads0) ->
    Keys = maps:keys(Cells0),
    Acc0 = #{
        cells => Cells0,
        begin_crossroad => BeginCrossroad0,
        end_crossroad => EndCrossroad0,
        crossroads => CrossRoads0,
        empty_cells_before => 0,
        no_cells = NoCells
    },

    lists:foldr(fun
        (CellId, AccIn) ->
            Cells = maps:get(cells, AccIn),
            case maps:get(CellId, Cells) of
                #cell{car = undefined} ->
                    maps:update_with(empty_cells_before, fun(V) -> V + 1 end, AccIn);
                Cell ->
                    case CellId of
%%                        LastCellNo ->
%%                            progress_car_on_ending_cell(CellId, Cell, AccIn);
                        0 ->
                            progress_car_on_starting_cell(CellId, Cell, AccIn);
                        _ ->
                            progress_car_on_cell(Cell, AccIn)
                    end
            end
    end, Acc0, Keys).

progress_car_on_starting_cell(CellId, Cell, AccIn) ->
    %TODo maybe AccIn should be record with well defined structure
    Cells = maps:get(cells, AccIn),
    StartCrossroad = maps:get(start_crossroad, AccIn),
    EndCrossroad = maps:get(end_crossroad, AccIn),
    Crossroads = maps:get(crossroads, AccIn),
    NoCells = maps:get(no_cells, AccIn),
    {UpdatedCell, UpdatedStartCrossroad, UpdatedEndCrossroad, UpdatedCrossroads} =
        progress_car_on_start(Cell, NoCells, StartCrossroad, EndCrossroad, Crossroads),
    AccIn#{
        cells => Cells#{CellId => UpdatedCell},
        start_crossroad => UpdatedStartCrossroad,
        end_crossroad => UpdatedEndCrossroad,
        crossroads => UpdatedCrossroads,
        empty_cells_before => 0
    }.

progress_car_on_cell(#cell{car=undefined}, AccIn) ->
    AccIn;
progress_car_on_cell(Cell, AccIn) ->
    Cells = maps:get(cells, AccIn),
    NoCells = maps:get(no_cells, AccIn),
    StartCrossroad = maps:get(start_crossroad, AccIn),
    EndCrossroad = maps:get(end_crossroad, AccIn),
    Crossroads = maps:get(crossroads, AccIn),
    EmptyCellsBefore = maps:get(empty_cells_before, AccIn),
    {UpdatedCells, UpdatedStartCrossroad, UpdatedEndCrossroad, UpdatedCrossroads} =
        progress_car(Cell, Cells, NoCells, StartCrossroad, EndCrossroad, Crossroads, EmptyCellsBefore),
    AccIn#{
        cells => UpdatedCells,
        start_crossroad => UpdatedStartCrossroad,
        end_crossroad => UpdatedEndCrossroad,
        crossroads => UpdatedCrossroads,
        empty_cells_before => 0
    }.

progress_car(Cell, Cells, NoCells, StartCrossroad, EndCrossroad, Crossroads, EmptyCellsBefore) ->

    CellId = Cell#cell.id,
    Velocity = get_car_velocity(Cell),
    NewVelocity = maybe_accelerate(Velocity),
    Car = get_car(Cell),
    {UpdatedCar, DeltaX} = case way_is_free(NewVelocity, EmptyCellsBefore) of
        % todo add changing lanes
        true ->
            {Car#car{velocity=NewVelocity}, NewVelocity div ?CAR_SIZE};
        false ->
            {Car#car{velocity=0}, EmptyCellsBefore}
    end,
    TargetCellId = CellId + DeltaX,
    TargetCell = maps:get(TargetCellId, Cells),
    SourceCell = Cell#cell{car=undefined},
    TargetCell2 = TargetCell#cell{car=UpdatedCar},
    Cells2 = Cells#{
        CellId => SourceCell,
        TargetCellId => TargetCell2
    },
    {Cells2, StartCrossroad, EndCrossroad, Crossroads, EmptyCellsBefore}.




%%get_car(#cell{car=undefined}) ->
get_car(#cell{car=Car}) ->
    Car.

get_car_velocity(#cell{car=undefined}) -> undefined;
get_car_velocity(#cell{car=#car{velocity = Velocity}}) -> Velocity.

maybe_accelerate(Velocity) ->
    min(?DEFAULT_SPEED_LIMIT, ?DEFAULT_ACCELERATION + Velocity).

way_is_free(Velocity, EmptyCellsBefore) ->
    EmptyCellsBefore * ?CAR_SIZE >= Velocity.


