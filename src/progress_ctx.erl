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
-export([get_fractions/1, get_fraction/2, get_cells/3, get_cell/4,
    get_fraction/1, set_cell_id/2, get_cell/1, get_cells/1,
    get_lane/3, get_lane/1, get_lanes/2, get_lanes/1, get_lane_id/1,
    get_fraction_id/1, get_cell_id/1, update_fraction/2, get_empty_cells_number/1,
    get_crossroad/1, init/6, init/4, get_fraction_ids/1, set_fraction_id/2,
    get_lane_ids/1, set_lane_id/2, count_empty_cells/1, get_fractions_no/1, update_fractions/2]).

-record(progress_ctx, {
    cell_id :: id(),
    lane_id :: id(),
    fraction_id :: id(),
    road_id :: id(),
    road_fractions :: #{id() => road_fraction()},
    empty_cells_before =#{} :: #{lane_id() => non_neg_integer()},
    crossroad :: crossroad(),
    no_fractions :: non_neg_integer()
}).

init(RoadId, RoadFractions, EndCrossRoad, NoFractions) ->
    #progress_ctx{
        road_id = RoadId,
        road_fractions = RoadFractions,
        crossroad = EndCrossRoad,
        no_fractions = NoFractions
%%        lane_extension_cells = count_lane_extension(LaneId, FractionId, RoadId)
    }.

init(LaneId, FractionId, RoadId, EndCrossRoad, RoadFractions, NoFractions) ->
    #progress_ctx{
        lane_id = LaneId,
        fraction_id = FractionId,
        road_id = RoadId,
        road_fractions = RoadFractions,
        crossroad = EndCrossRoad,
        no_fractions = NoFractions
%%        lane_extension_cells = count_lane_extension(LaneId, FractionId, RoadId)
    }.

set_cell_id(CellId, Ctx) ->
    Ctx#progress_ctx{cell_id = CellId}.

set_lane_id(LaneId, Ctx) ->
    Ctx#progress_ctx{lane_id = LaneId}.

set_fraction_id(FractionId, Ctx) ->
    Ctx#progress_ctx{fraction_id = FractionId}.

update_fraction(NewRoadFraction = #road_fraction{id=Id},
    Ctx=#progress_ctx{road_fractions=RoadFractions}
) ->
    Ctx#progress_ctx{road_fractions = RoadFractions#{Id =>NewRoadFraction}}.

update_fractions(NewRoadFractions, Ctx) ->
    Ctx#progress_ctx{road_fractions = NewRoadFractions}.


get_cell_id(#progress_ctx{cell_id=CellId}) ->
    CellId.

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

get_lane_id(#progress_ctx{lane_id=LaneId}) ->
    LaneId.

get_lane(LaneId, FractionId, Ctx) ->
    maps:get(LaneId, get_lanes(FractionId, Ctx)).

get_lane(Ctx = #progress_ctx{lane_id = LaneId, fraction_id = FractionId}) ->
    get_lane(LaneId, FractionId, Ctx).

get_lane_ids(#progress_ctx{road_fractions = #road_fraction{lanes = Lanes}}) ->
    maps:keys(Lanes).

get_lanes(FractionId, Ctx) ->
    #road_fraction{lanes = Lanes} = get_fraction(FractionId, Ctx),
    Lanes.

get_lanes(Ctx = #progress_ctx{fraction_id = FractionId}) ->
    get_lanes(FractionId, Ctx).

get_fraction_id(#progress_ctx{fraction_id= FractionId}) ->
    FractionId.

get_fraction(FractionId, Ctx) ->
    maps:get(FractionId, get_fractions(Ctx)).

get_fraction(Ctx =#progress_ctx{fraction_id = FractionId}) ->
    get_fraction(FractionId, Ctx).

get_fraction_ids(#progress_ctx{road_fractions = RoadFractions}) ->
    maps:keys(RoadFractions).

get_fractions(#progress_ctx{road_fractions = RoadFractions}) ->
    RoadFractions.

get_fractions_no(#progress_ctx{no_fractions = NoFractions}) ->
    NoFractions.

get_crossroad(#progress_ctx{crossroad = Crossroad}) ->
    Crossroad.

get_empty_cells_number(Ctx = #progress_ctx{
    lane_id = LaneId,
    empty_cells_before = EmptyCells
}) ->
    #road_fraction{special_rules = SpecialRules} = get_fraction(Ctx),
    case maps:is_key(LaneId, SpecialRules) of
        false ->
            maps:get(LaneId, EmptyCells);
        _ ->
            0
    end.

count_empty_cells(#progress_ctx{
    road_id = RoadId,
    crossroad = XRoad =
        #crossroad{
            width = Width,
            length = Length
}}) ->
    case RoadId rem 2 == 0 of
        true ->
            lists:foldr(fun() ->
                ok %todo
            end, #{}, lists:seq(0, Width - 1))
    end.