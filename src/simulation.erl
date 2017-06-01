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
-export([change_lane/2]).


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
%%% In one tour, car can maximally move from road to crossroad or from
%%% crossroad to road. It can't move road->crossroad->road nor
%%% crossroad->road->crossroad.
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
    begin_crossroad = BeginCrossroadId,
    end_crossroad = EndCrossroadId
}, Crossroads0) ->

    BeginCrossroad0 = maps:get(BeginCrossroadId, Crossroads0),
    EndCrossroad0 = maps:get(EndCrossroadId, Crossroads0),

    {SideFalling, EndCrossroad1, BeginCrossroad1} =
        progress_cars_on_side(SideFalling0, EndCrossroad0, BeginCrossroad0),
    {SideRising, BeginCrossroad2, EndCrossroad2} =
        progress_cars_on_side(SideRising0, BeginCrossroad1, EndCrossroad1),

    {
        Road#road{
            side_falling = SideFalling,
            side_rising = SideRising,
            begin_crossroad = BeginCrossroad2,
            end_crossroad = EndCrossroad2
        },
        Crossroads0#{
            BeginCrossroadId => BeginCrossroad2,
            EndCrossroadId => EndCrossroad2
        }
    }.


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_side(RoadFractions0, BeginCrossroad0, EndCrossroad0) ->
    RoadFractionsIds = maps:keys(RoadFractions0),
    lists:foldr(fun(FractionId, {RoadFractions, BeginCrossroad, EndCrossroad}) ->
            progress_cars_on_road_fraction(FractionId, RoadFractions, BeginCrossroad, EndCrossroad)
    end, {RoadFractions0, BeginCrossroad0, EndCrossroad0}, lists:sort(RoadFractionsIds)).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_road_fraction(FractionId, RoadFractions0, BeginCrossroad0, EndCrossroad0) ->
    #road_fraction{lanes = Lanes0}= maps:get(FractionId, RoadFractions0),
    LaneIds = maps:keys(Lanes0),
    lists:foldr(fun(LaneId, {RoadFractions, BeginCrossroad, EndCrossroad}) ->
        progress_cars_on_lane(LaneId, FractionId, RoadFractions, BeginCrossroad, EndCrossroad)
    end, {RoadFractions0, BeginCrossroad0, EndCrossroad0}, lists:sort(LaneIds)).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
%%progress_cars_on_lane(Lane = #lane{id= LaneId, cells=Cells0, no_cells = NoCells}, Lanes0,
progress_cars_on_lane(LaneId, FractionId, RoadFractions0, BeginCrossroad0,
    EndCrossroad0
) ->
    ProgressCtx0 = progress_ctx:init(LaneId, FractionId, BeginCrossroad0, EndCrossroad0, RoadFractions0),
    Cells = progress_ctx:get_cells(ProgressCtx0),

    lists:foldr(fun(CellId, Ctx) ->
        Ctx2 = progress_ctx:set_cell_id(CellId, Ctx),
        Cell = progress_ctx:get_cell(Ctx2),
        case is_occupied(Cell) of
            false ->
                Ctx2;
            _ ->
                case change_lane(LaneId, Ctx) of
%%                    TODO use ctx in these functions
                    no_change ->
                        try_move_forward(CellId, LaneId, FractionId, RoadFractions);
                    left ->
                        try_change_lane_to_left(CellId, LaneId, FractionId, RoadFractions);
                    right ->
                        try_change_lane_to_right(CellId, LaneId, FractionId, RoadFractions)
                end
        end
    end, ProgressCtx0, lists:sort(maps:keys(Cells))).

is_occupied(#cell{car = undefined}) -> false;
is_occupied(#cell{car = _}) -> true.

try_change_lane_to_left(CellId, SrcLaneId, FractionId, RoadFractions) ->
    try_change_lane(CellId, SrcLaneId, FractionId, RoadFractions, -1).

try_change_lane_to_right(CellId, SrcLaneId, FractionId, RoadFractions) ->
    try_change_lane(CellId, SrcLaneId, FractionId, RoadFractions, +1).

try_change_lane(CellId, SrcLaneId, FractionId, RoadFractions, Direction) ->
    TargetLaneId = SrcLaneId + Direction,
    RoadFraction = #road_fraction{lanes=Lanes} = maps:get(FractionId, RoadFractions),
    SrcLane = #lane{cells = SrcCells} = maps:get(SrcLaneId, Lanes),
    TargetLane = #lane{cells = TargetCells} = maps:get(TargetLaneId, Lanes),
    SrcCell = #cell{car = Car} = maps:get(CellId, SrcCells),
    TargetCell = maps:get(CellId, TargetCells),
    case is_occupied(TargetCell) of
        false ->
            SrcCell2 = SrcCell#cell{car = undefined},
            SrcCells2 = TargetCell#{CellId => SrcCell2},
            SrcLane2 = SrcLane#lane{cells = SrcCells2},
            TargetCell2 = TargetCell#cell{car = Car},
            TargetCells2 = TargetCell#{CellId => TargetCell2},
            TargetLane2 = TargetLane#lane{cells = TargetCells2},
            Lanes2 = Lanes#{
                SrcLaneId => SrcLane2,
                TargetLaneId => TargetLane2
            },
            RoadFraction#road_fraction{lanes = Lanes2};
        _ ->
            try_move_forward(CellId, SrcLaneId, FractionId, RoadFractions)
    end.


try_move_forward(CellId, LaneId, FractionId, RoadFractions) ->
    #road_fraction{lanes = Lanes} = maps:get(FractionId, RoadFractions),
    #lane{cells = Cells} = maps:get(LaneId, Lanes),

    CellIds = maps:keys(Cells),
    ok.


change_lane(Id, Ctx) ->
    Lanes = progress_c
    LastLane =  length(maps:keys(Lanes)) - 1,
    case Id of
        0 -> change_to_right_or_left(false, true);
        LastLane -> change_to_right_or_left(true, false);
        _ -> change_to_right_or_left(true, true)
    end.


change_to_right_or_left(false, false) -> no_change;
change_to_right_or_left(false, _) ->
    case random:uniform() < ?CHANGE_LANE_PROBABILITY of
        true -> right;
        _ -> no_change
    end;
change_to_right_or_left(_, false) ->
    case random:uniform() < ?CHANGE_LANE_PROBABILITY of
        true -> left;
        _ -> no_change
    end;
change_to_right_or_left(_, _) ->
    Random = random:uniform(),
    case  Random < ?CHANGE_LANE_PROBABILITY of
        false -> no_change;
        true -> case Random < ?CHANGE_LANE_PROBABILITY / 2 of
            true -> left;
            _ -> right
        end
    end.


num_of_lanes(FractionId, RoadFractions) ->
    {#road_fraction{
        no_lanes = NoLanes
    }} = maps:get(FractionId, RoadFractions),
    NoLanes.



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
        true ->
            {Car#car{velocity=NewVelocity}, NewVelocity div ?CAR_SIZE};
        false ->
            {Car#car{velocity=0}, EmptyCellsBefore}
    end,
    TargetCellId = CellId + DeltaX,
    TargetCell = maps:get(TargetCellId, Cells),
    SourceCell = Cell#cell{car = undefined},
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


