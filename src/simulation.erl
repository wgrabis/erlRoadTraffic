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
%%-export([change_lane/2]).


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
    id = RoadId0,
    side_falling = SideFalling0,
    side_rising = SideRising0,
    begin_crossroad = BeginCrossroadId,
    end_crossroad = EndCrossroadId,
    no_fractions = NoFractions
}, Crossroads0) ->

    BeginCrossroad0 = maps:get(BeginCrossroadId, Crossroads0),
    EndCrossroad0 = maps:get(EndCrossroadId, Crossroads0),

    FallingCtx = progress_ctx:init(RoadId0, SideFalling0, BeginCrossroad0, NoFractions),
    RisingCtx = progress_ctx:init(RoadId0, SideRising0, EndCrossroad0, NoFractions),

    FallingCtx2 = progress_cars_on_side(FallingCtx),
    RisingCtx2 = progress_cars_on_side(RisingCtx),

    {
        Road#road{
            side_falling = progress_ctx:get_fractions(FallingCtx2),
            side_rising = progress_ctx:get_fractions(RisingCtx2)
        },
        Crossroads0#{
            BeginCrossroadId => progress_ctx:get_crossroad(FallingCtx2),
            EndCrossroadId => progress_ctx:get_crossroad(RisingCtx2)
        }
    }.


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_side(Ctx0) ->
    RoadFractionsIds = progress_ctx:get_fraction_ids(Ctx0),
    Ctx2 = progress_ctx:count_empty_cells(Ctx0),     %todo count empty cells for each lane of last fraction of given side
    lists:foldr(fun(FractionId, Ctx) ->
        progress_cars_on_road_fraction(FractionId, Ctx)
    end, Ctx2, lists:sort(RoadFractionsIds)).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_road_fraction(FractionId, Ctx0) ->
    Ctx1 = progress_ctx:set_fraction_id(FractionId, Ctx0),
    LaneIds = progress_ctx:get_lane_ids(Ctx1),
    lists:foldr(fun(LaneId, Ctx) ->
        progress_cars_on_lane(LaneId, Ctx)
    end, Ctx1, lists:sort(LaneIds)).


%%%-------------------------------------------------------------------
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
progress_cars_on_lane(LaneId, Ctx0) ->
    Ctx1 = progress_ctx:set_lane_id(LaneId, Ctx0),
    Cells = progress_ctx:get_cells(Ctx1),

    lists:foldr(fun(CellId, Ctx) ->
        Ctx2 = progress_ctx:set_cell_id(CellId, Ctx),
        Cell = progress_ctx:get_cell(Ctx2),
        case is_occupied(Cell) of
            false ->
                Ctx2;
            _ ->
                case change_lane(Ctx) of
%%                    TODO use ctx in these functions
                    no_change ->
                        try_move_forward(Ctx);
                    left ->
                        try_change_lane_to_left(Ctx);
                    right ->
                        try_change_lane_to_right(Ctx)
                end
        end
    end, Ctx1, lists:sort(maps:keys(Cells))).

is_occupied(#cell{car = undefined}) -> false;
is_occupied(#cell{car = _}) -> true.

try_change_lane_to_left(Ctx) ->
    try_change_lane(Ctx, -1).

try_change_lane_to_right(Ctx) ->
    try_change_lane(Ctx, +1).

try_change_lane(Ctx, Direction) ->
    CellId = progress_ctx:get_cell_id(Ctx),
    SrcCell = #cell{car = Car} = progress_ctx:get_cell(Ctx),
    SrcLaneId = progress_ctx:get_lane_id(Ctx),
    SrcLane = #lane{cells = SrcCells} = progress_ctx:get_lane(Ctx),
    Lanes = progress_ctx:get_lanes(Ctx),
    RoadFraction = progress_ctx:get_fraction(Ctx),
    TargetLaneId = SrcLaneId + Direction,
    TargetLane = #lane{cells = TargetCells} = maps:get(TargetLaneId, Lanes),
    TargetCell = maps:get(CellId, TargetCells),
    case is_occupied(TargetCell) of
        false ->
            SrcCell2 = SrcCell#cell{car = undefined},
            SrcCells2 = SrcCells#{CellId => SrcCell2},
            SrcLane2 = SrcLane#lane{cells = SrcCells2},
            TargetCell2 = TargetCell#cell{car = Car},
            TargetCells2 = TargetCell#{CellId => TargetCell2},
            TargetLane2 = TargetLane#lane{cells = TargetCells2},
            Lanes2 = Lanes#{
                SrcLaneId => SrcLane2,
                TargetLaneId => TargetLane2
            },
            NewRoadFraction = RoadFraction#road_fraction{lanes = Lanes2},
            progress_ctx:update_fraction(NewRoadFraction, Ctx);
        _ ->
            try_move_forward(Ctx)
    end.

change_lane(Ctx) ->
    Lanes = progress_ctx:get_lane(Ctx),
    LastLane =  length(maps:keys(Lanes)) - 1,
    LaneId = progress_ctx:get_lane_id(Ctx),
    case LaneId of
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

try_move_forward(Ctx) ->
    SrcCellId = progress_ctx:get_cell_id(Ctx),
    SrcCell = #cell{car = Car} = progress_ctx:get_cell(Ctx),
    SrcLaneId = progress_ctx:get_lane_id(Ctx),
    SrcLane = #lane{cells = SrcCells} = progress_ctx:get_lane(Ctx),
    SrcLanes = progress_ctx:get_lanes(Ctx),
    FractionId = progress_ctx:get_fraction_id(Ctx),
    SrcFraction = #road_fraction{special_rules = SpecialRules} =
        progress_ctx:get_fraction(Ctx),
    RoadFractions = progress_ctx:get_fractions(Ctx),
    Velocity = get_car_velocity(SrcCell),
    NewVelocity = maybe_accelerate(Velocity),
    Car = get_car(SrcCell),
    EmptyCellsBefore = progress_ctx:get_empty_cells_number(Ctx),
    NoFractions = progress_ctx:get_fractions_no(Ctx),


    {UpdatedCar, DeltaX} = case way_is_free(NewVelocity, EmptyCellsBefore) of
        true ->
            {Car#car{velocity=NewVelocity}, NewVelocity div ?CAR_SIZE};
        false ->
            {Car#car{velocity=0}, EmptyCellsBefore}
    end,

    case DeltaX of
        0 ->
            Ctx;
        _ ->
            %% todo refactor, extract to separate function            
            TargetCellId = SrcCellId + DeltaX,
            NoFractions = progress_ctx:get_fractions_no(Ctx),
            case TargetCellId - NoFractions of
                TargetCellId2 when TargetCellId2 < 0 ->
                    % move in range of same road_fraction
                    SrcCell2 = SrcCell#cell{car = undefined},
                    TargetCell = maps:get(TargetCell, SrcCells),
                    TargetCell2 = TargetCell#cell{car = UpdatedCar},
                    SrcCells2 = SrcCells#{
                        SrcCellId => SrcCell2,
                        TargetCellId => TargetCell2
                    },
                    SrcLane2 = SrcLane#lane{cells = SrcCells2},
                    Lanes2 = SrcLanes#{SrcLaneId => SrcLane2},
                    NewRoadFraction = SrcFraction#road_fraction{lanes = Lanes2},
                    progress_ctx:update_fraction(NewRoadFraction, Ctx);
                TargetCellId2 ->
                    case FractionId == NoFractions - 1 of
                        true ->
                            move_car_to_xroad(UpdatedCar, Ctx);
                        _ ->
                            TargetFraction = #road_fraction{lanes = TargetLanes}
                                = maps:get(FractionId + 1, RoadFractions),
                            TargetLaneId = maps:get(SrcLaneId, SpecialRules, SrcLaneId),
                            TargetLane = #lane{cells = TargetCells}
                                = maps:get(TargetLaneId, TargetLanes),
                            TargetCell = maps:get(TargetCellId2, TargetCells),

                            SrcCell2 = SrcCell#cell{car = undefined},
                            TargetCell2 = TargetCell#cell{car = UpdatedCar},
                            SrcCells2 = SrcCells#{SrcCellId => SrcCell2},
                            TargetCells2 = TargetCells#{TargetCellId2 => TargetCell2},
                            SrcLane2 = SrcLane#lane{cells = SrcCells2},
                            TargetLane2 = TargetLane#lane{cells = TargetCells2},
                            SrcLanes2 = SrcLanes#{SrcLaneId => SrcLane2},
                            TargetLanes2 = TargetLanes#{TargetLaneId => TargetLane2},
                            SrcFraction2 = SrcFraction#road_fraction{lanes = SrcLanes2},
                            TargetFraction2 = TargetFraction#road_fraction{lanes = TargetLanes2},
                            RoadFractions2 = #{
                                FractionId => SrcFraction2,
                                FractionId + 1 => TargetFraction2
                            },
                            progress_ctx:update_fractions(RoadFractions2, Ctx)
                    end
            end
    end.
    



get_car(#cell{car=Car}) ->
    Car.

get_car_velocity(#cell{car=undefined}) -> undefined;
get_car_velocity(#cell{car=#car{velocity = Velocity}}) -> Velocity.

maybe_accelerate(Velocity) ->
    min(?DEFAULT_SPEED_LIMIT, ?DEFAULT_ACCELERATION + Velocity).

way_is_free(Velocity, EmptyCellsBefore) ->
    EmptyCellsBefore * ?CAR_SIZE >= Velocity.


move_car_to_xroad(UpdatedCar, Ctx) ->
    % todo
    Ctx.