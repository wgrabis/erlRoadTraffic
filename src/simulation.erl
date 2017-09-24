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
    maps:fold(fun(N, XRoad, {CurrRoads, CurrCrossroads}) ->
        {UpdatedRoads, UpdatedCrossroad} = progress_cars_on_xroad(CurrRoads, XRoad, Timestamp),
        {UpdatedRoads, maps:update(N, UpdatedCrossroad, CurrCrossroads)}
    end,{Roads, Crossroads}, Crossroads).



progress_cars_on_xroad(Roads, Crossroad = #crossroad{
    id = Id,
    cells = Cells,
    roads = AdjRoads,
    length = Length,
    width = Width
}, Timestamp) ->

    CrossRoadData = gen_xroad_lane_data_map(Roads, AdjRoads, Id),

    case Timestamp rem 2 of
        1 ->
            progress_cars_on_xroad_vertical(Roads, Crossroad, CrossRoadData);
        0 ->
            progress_cars_on_xroad_horizontal(Roads, Crossroad, CrossRoadData)
    end.


progress_cars_on_xroad_vertical(Roads, Crossroad, CrossRoadData) ->
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
    {UpdatedRoads, UpdatedCrossroad} = lists:foldl(
        fun(N, {CurrRoads, CurrCrossroad}) ->
            Col = crossroad_helpers:get_column_crossroad(N, Crossroad, rising),
            progress_xroad_row(Col, CurrRoads, CurrCrossroad)
        end, {Roads, Crossroad}, lists:seq(0, InLanes - 1)),

    %% todo left turn
    lists:foldl(
        fun(N, {CurrRoads, CurrCrossroad}) ->
            Col = crossroad_helpers:get_column_crossroad(N, Crossroad, falling),
            progress_xroad_row(Col, CurrRoads, CurrCrossroad)
        end, {UpdatedRoads, UpdatedCrossroad}, lists:seq(InLanes, InLanes + OutLanes - 1)).


progress_cars_on_xroad_horizontal(Roads, Crossroad, CrossRoadData) ->
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
    {UpdatedRoads, UpdatedCrossroad} = lists:foldl(
        fun(N, {CurrRoads, CurrCrossroad}) ->
            Row = crossroad_helpers:get_row_crossroad(N, Crossroad, falling),
            progress_xroad_row(Row, CurrRoads, CurrCrossroad)
        end, {Roads, Crossroad}, lists:seq(0, OutLanes -1)),

    %% todo left turn
    lists:foldl(
        fun(N, {CurrRoads, CurrCrossroad}) ->
            Row = crossroad_helpers:get_row_crossroad(N, Crossroad, rising),
            progress_xroad_row(Row, CurrRoads, CurrCrossroad)
        end, {UpdatedRoads, UpdatedCrossroad}, lists:seq(OutLanes, InLanes + OutLanes -1)).


progress_xroad_row(Row, Roads, Crossroad) ->
    lists:foldl(fun(X, {CurrRoads, CurrCrossroad}) ->
                progress_cell_on_xroad_row(X, CurrRoads, CurrCrossroad)
                end, {Roads, Crossroad}, Row).


progress_cell_on_xroad_row(CellId, Roads, Crossroad) ->
    XroadId = Crossroad#crossroad.id,
    Cell = #cell{car = Car = #car{
            velocity = Velocity,
            target_cell = Target
    }} = maps:get(CellId, Crossroad#crossroad.cells),
    CellWay = crossroad_helpers:get_cells_to_target(CellId, Target,Crossroad),
    {Taken, LeftDistance, LastCellN} = progress_cell_on_xroad_row_helper(CellWay, Velocity, Crossroad),
    case Taken of
        true ->
            {Roads, Crossroad};
        false ->
            UpdatedCrossroad = Crossroad#crossroad{
                cells = maps:update(CellId, #cell{
                    id = CellId,
                    car = undefined
                }, Crossroad#crossroad.cells)
            },
        case LeftDistance of
            0 ->
                {Roads, UpdatedCrossroad#crossroad{cells =
                maps:update(LastCellN, Cell#cell{id=LastCellN}, Crossroad#crossroad.cells)}};
            _ ->
                case Roads of
                    undefined ->
                        {Roads, Crossroad#crossroad{
                            cells = maps:update(LastCellN, #cell{id = LastCellN, car = Car}, Crossroad#crossroad.cells)}
                        };
                    _ ->
                        {RoadId, LaneId} = crossroad_helpers:get_target(LastCellN, Crossroad),
                        Road = #road{
                            begin_crossroad = BeginXroad,
                            end_crossroad = EndXroad,
                            no_fractions = NoFractions,
                            side_rising = SideRising,
                            side_falling = SideFalling
                        } = maps:get(RoadId, Roads),
                        ProgressCtx = case XroadId of
                            BeginXroad ->
                                progress_ctx:init(LaneId, 0, RoadId, SideRising, EndXroad, NoFractions);
                            EndXroad ->
                                progress_ctx:init(LaneId, 0, RoadId, SideFalling, BeginXroad, NoFractions)
                        end,
                        ProgressCtx2 = progress_ctx:count_empty_cells(ProgressCtx),
                        #lane{no_cells = NoCells} = progress_ctx:get_lane(ProgressCtx2),
                        ProgressCtx3 = progress_ctx:set_cell_id(NoCells - 1, ProgressCtx2),
                        case progress_ctx:is_empty(ProgressCtx3) of
                            true ->
                                ProgressCtx4 = progress_ctx:set_car(Car, ProgressCtx3),
                                ProgressCtx5 = try_move_forward(ProgressCtx4),
                                {
                                    Roads#{RoadId => Road#road{
                                        side_rising = progress_ctx:get_fraction(ProgressCtx5)
                                    }},
                                    Crossroad#crossroad{
                                        cells = maps:update(LastCellN, #cell{id = LastCellN, car = undefined}, Crossroad#crossroad.cells)
                                    }
                                };
                            false ->
                                {Roads, Crossroad#crossroad{
                                    cells = maps:update(LastCellN, #cell{id = LastCellN, car = Car}, Crossroad#crossroad.cells)
                                }}
                        end
                end
        end
    end.




progress_cell_on_xroad_row_helper([], CurrDistance, Crossroad) ->
    {true, CurrDistance, undefined};

progress_cell_on_xroad_row_helper([N | Tail], CurrDistance, Crossroad) ->
    CurrCell = maps:get(N,Crossroad#crossroad.cells),
    case CurrCell#cell.car of
        undefined ->
            {Taken, LeftDistance, LastCellN} = progress_cell_on_xroad_row_helper(Tail, CurrDistance - 1, Crossroad),
            case Taken of
                true ->
                    {false, CurrDistance, N};
                false ->
                    {false, LeftDistance, LastCellN}
            end;
        _ ->
            {true, 0, undefined}
    end.

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
          NoLanesRising = (maps:get(0, RisingFractions))#road_fraction.no_lanes,
          NoLanesFalling = (maps:get(NoFractions - 1, FallingFractions))#road_fraction.no_lanes,
          case XRoadId of
              BeginX ->
                  maps:put(Num, #lanes_data{in_no_lanes = NoLanesFalling, out_no_lanes = NoLanesRising}, CurrLaneMap);
              EndX ->
                  maps:put(Num, #lanes_data{out_no_lanes = NoLanesFalling, in_no_lanes = NoLanesRising}, CurrLaneMap)
          end
      end, {#{}}, AdjRoads
    ).

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


move_car_to_xroad(Car = #car{velocity = Velocity}, Ctx) ->
    Crossroad = progress_ctx:get_crossroad(Ctx),
    Rules = progress_ctx:get_crossroad_lane_rules(Ctx),
    TurnRule = choose_turn_rule(Rules),
    case get_begin_cell(Ctx, Crossroad) of
        none ->
            Ctx;
        BeginCell ->
            UpdatedCar = Car#car{
                velocity = Velocity - 1,
                target_cell = crossroad_helpers:count_target_cell(TurnRule, BeginCell, Crossroad)
            },
            UpdatedCrossroad = Crossroad#crossroad{
                cells = maps:update(
                    BeginCell,
                    BeginCell#cell{car = UpdatedCar},
                    Crossroad#crossroad.cells)
            },
            {_, ProgressedCrossroad} = progress_cell_on_xroad_row(BeginCell, undefined, UpdatedCrossroad),
            progress_ctx:set_xroad(ProgressedCrossroad, Ctx)
    end.

choose_turn_rule([]) ->
    none;

choose_turn_rule([Rule |Tail]) ->
    case choose_turn_rule(Tail) of
        none ->
            Rule;
        Val ->
            Val
    end.


get_begin_cell(Ctx, Crossroad) ->
    LaneId = progress_ctx:get_lane(Ctx),
    Fraction = progress_ctx:get_fraction(Ctx),
    RoadId = progress_ctx:get_road_id(Ctx),
    CellNo = crossroad_helpers:get_begin_cell_number(LaneId, Fraction, RoadId, Crossroad),
    #cell{
        car = CurrCar
    } = maps:get(CellNo, Crossroad#crossroad.cells),
    case CurrCar of
        undefined ->
            CellNo;
        _ ->
            none
    end.

