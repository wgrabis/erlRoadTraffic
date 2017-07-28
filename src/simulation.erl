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
-include("progress.hrl").
-include("constants.hrl").

%% API
%%-export([change_lane/2]).
-export([simulate/1]).



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
progress_cars_on_xroads(Roads, Crossroads, Timestamp) ->
    erlang:error(not_implemented).



progress_cars_on_xroad(Roads, Crossroad = #crossroad{
    id = Id,
    cells = Cells,
    roads = AdjRoads,
    length = Length,
    width = Width,
    empty_cells_before = EmptyCells
}, Timestamp) ->

    RoadFreeSpace = gen_free_space_map(Roads, AdjRoads, Id),
    CrossRoadData = gen_xroad_lane_data_map(Roads, AdjRoads, Id),

    case Timestamp rem 2 of
        1 ->
            EndCells = progress_cars_on_xroad_vertical(Roads, Crossroad, RoadFreeSpace, CrossRoadData, Cells);
        0 ->
            EndCells = progress_cars_on_xroad_horizontal(Roads, Crossroad, RoadFreeSpace, CrossRoadData, Cells)
    end,


    Crossroad#crossroad{cells = EndCells}.



progress_cars_on_xroad_horizontal(Roads, Crossroad, RoadFreeSpace, CrossRoadData, Cells) ->
    {#lanes_data{in_no_lanes = In, out_no_lanes = Out}, Which} = crossroad_helpers:lanes_data_compare(
        maps:get(<<0>>, CrossRoadData), maps:get(<<2>>, CrossRoadData,crossroad_helpers:empty_lane_data())),
    case Which of
        0 ->
            InLanes = In,
            OutLanes = Out;
        1 ->
            InLanes = Out,
            OutLanes = In
    end,
    UpdatedCells = lists:foldl(
        fun(N, {CurrCells}) ->
            RowList = crossroad_helpers:get_row_crossroad(N, Crossroad, falling),
            progress_cells_on_xroad(RowList, Roads, Crossroad, RoadFreeSpace, CurrCells)
        end, {Cells}, lists:seq(0, OutLanes -1)),
    lists:foldl(
        fun(N, {CurrCells}) ->
            RowList = crossroad_helpers:get_row_crossroad(N, Crossroad, rising),
            progress_cells_on_xroad(RowList, Roads, Crossroad, RoadFreeSpace, CurrCells)
        end, {UpdatedCells}, lists:seq(OutLanes, InLanes + OutLanes -1)).


progress_cells_on_xroad(CellList, Roads, Crossroad, RoadFreeSpace, Cells) ->
    .


progress_cars_on_xroad_vertical(Roads, Crossroad, RoadFreeSpace, CrossRoadData, Cells) ->
    {#lanes_data{in_no_lanes = In, out_no_lanes = Out}, Which} = crossroad_helpers:lanes_data_compare(
        maps:get(<<1>>, CrossRoadData, crossroad_helpers:empty_lane_data()), maps:get(<<3>>, CrossRoadData,crossroad_helpers:empty_lane_data())),
    case Which of
        0 ->
            InLanes = In,
            OutLanes = Out;
        1 ->
            InLanes = Out,
            OutLanes = In
    end,
    UpdatedCells = lists:foldl(
        fun(N, {CurrCells}) ->
            ColList = crossroad_helpers:get_column_crossroad(N, Crossroad, rising),
            progress_cells_on_xroad(ColList, Roads, Crossroad, RoadFreeSpace, CurrCells)
        end, {Cells}, lists:seq(0, InLanes -1)),
    lists:foldl(
        fun(N, {CurrCells}) ->
            ColList = crossroad_helpers:get_column_crossroad(N, Crossroad, falling),
            progress_cells_on_xroad(ColList, Roads, Crossroad, RoadFreeSpace, CurrCells)
        end, {UpdatedCells}, lists:seq(InLanes, InLanes + OutLanes -1)).

progress_xroad_operation({CX, CY}, Car, Crossroad, Roads) ->
    {}.


gen_free_space_map(Roads, IdMap, XRoadId) ->
    maps:fold(
        fun(Num, RoadId, {CurrFreeMap}) ->
            #road{
                begin_crossroad = BeginX,
                end_crossroad = EndX,
                empty_cells_before_falling = FreeSpaceFalling,
                empty_cells_before_rising = FreeSpaceRising
            } = maps:get(RoadId, Roads),
            case XRoadId of
                BeginX ->
                    FreeSpace = FreeSpaceRising;
                EndX ->
                    FreeSpace = FreeSpaceFalling
            end,
            maps:put(Num, FreeSpace, CurrFreeMap)
        end, {#{}}, IdMap
    ).

gen_xroad_lane_data_map(Roads, AdjRoads, XRoadId) ->
    maps:fold(
      fun(Num, RoadId, {CurrLaneMap}) ->
          #road{
              begin_crossroad = BeginX,
              end_crossroad = EndX,
              side_falling = FallingFractions,
              side_rising = RisingFractions,
              no_fractions = NoFractions
          } = maps:get(RoadId, Roads),
          NoLanesRising = maps:get(0, RisingFractions)#road_fraction.no_lanes,
          NoLanesFalling = maps:get(NoFractions - 1, FallingFractions)#road_fraction.no_lanes,
          case XRoadId of
              BeginX ->
                  maps:put(Num, #lanes_data{in_no_lanes = NoLanesFalling, out_no_lanes = NoLanesRising}, CurrLaneMap);
              EndX ->
                  maps:put(Num, #lanes_data{out_no_lanes = NoLanesFalling, in_no_lanes = NoLanesRising}, CurrLaneMap)
          end
      end, {#{}}, AdjRoads
    ).




split_vec_pos(Val1, Val2) when Val1 > Val2->

split_vec_pos(Val1, Val2)



get_to_target_cells(N, Target, Crossroad = #crossroad{
    cells = Cells,
    width = Width,
    length = Length
}, XLaneData) ->
    {X1, Y1} = progress_ctx:id_to_position(N, Width),
    {X2, Y2} = progress_ctx:id_to_position(Target, Width),
    case Y1 == Y2 of
        true ->
            split_vec_pos(X1, X2, Cells);
        false ->
            case X1 == X2 of
                true ->
                    split_vec_map(Y1, Y2, get_column_crossroad(X1, Crossroad));
                false ->
                    erlang:error(not_implemented)
            end
    end.


%%%-------------------------------  ------------------------------------
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
                case change_lane(Ctx2) of
                    no_change ->
                        try_move_forward(Ctx2);
                    left ->
                        try_change_lane_to_left(Ctx2);
                    right ->
                        try_change_lane_to_right(Ctx2)
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
    Lanes = progress_ctx:get_lanes(Ctx),
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
    EmptyCellsBefore = progress_ctx:get_empty_cells_number(Ctx),
    NoFractions = progress_ctx:get_fractions_no(Ctx),


    {UpdatedCar, DeltaX} = case way_is_free(NewVelocity, EmptyCellsBefore) of
        true ->
            {Car#car{velocity=NewVelocity}, NewVelocity};
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
                    TargetCell = maps:get(TargetCellId, SrcCells),
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