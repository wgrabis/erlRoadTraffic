%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(progress_ctx).
-author("Jakub Kudzia").


-include("model.hrl").
-include("constants.hrl").


%% API
-export([init/5, get_fractions/1, get_fraction/2, get_cells/3, get_cell/4,
    get_fraction/1, set_cell_id/2, get_cell/1, get_cells/1,
    get_lane/3, get_lane/1, get_lanes/2,
    get_lanes/1
]).

-record(progress_ctx, {
    cell_id :: id(),
    lane_id :: id(),
    fraction_id :: id(),
    road_fractions :: #{id() => road_fraction()},
    empty_cells_before = 0 :: non_neg_integer(),
    begin_crossroad :: crossroad(),
    end_crossroad :: crossroad()
}).


init(LaneId, FractionId, BeginCrossroad, EndCrossRoad, RoadFractions) ->
    #progress_ctx{
        lane_id = LaneId,
        fraction_id = FractionId,
        road_fractions = RoadFractions,
        begin_crossroad = BeginCrossroad,
        end_crossroad = EndCrossRoad
    }.

set_cell_id(CellId, Ctx) ->
    Ctx#progress_ctx{cell_id = CellId}.

get_cell(CellId, LaneId, FractionId, Ctx) ->
    maps:get(CellId, get_cells(LaneId, FractionId, Ctx)).

get_cell(Ctx = #progress_ctx{
    cell_id = CellId,
    lane_id = LaneId,
    fraction_id = FractionId}
) ->
    get_cell(CellId, LaneId, FractionId, Ctx).

get_cells(LaneId, FractionId, Ctx) ->
    #lane{cells = Cells} = get_lane(LaneId, FractionId, Ctx),
    Cells.

get_cells(Ctx = #progress_ctx{lane_id = LaneId, fraction_id = FractionId}) ->
    get_cells(LaneId, FractionId, Ctx).

get_lane(LaneId, FractionId, Ctx) ->
    maps:get(LaneId, get_lanes(FractionId, Ctx)).

get_lane(Ctx = #progress_ctx{lane_id = LaneId, fraction_id = FractionId}) ->
    get_lane(LaneId, FractionId, Ctx).

get_lanes(FractionId, Ctx) ->
    #road_fraction{lanes = Lanes} = get_fraction(FractionId, Ctx),
    Lanes.

get_lanes(Ctx = #progress_ctx{fraction_id = FractionId}) ->
    get_lanes(FractionId, Ctx).

get_fraction(FractionId, Ctx) ->
    maps:get(FractionId, get_fractions(Ctx)).

get_fraction(Ctx =#progress_ctx{fraction_id = FractionId}) ->
    get_fraction(FractionId, Ctx).

get_fractions(#progress_ctx{road_fractions = RoadFractions}) ->
    RoadFractions.

