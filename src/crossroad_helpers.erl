%%%-------------------------------------------------------------------
%%% @author wg
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jun 2017 03:32
%%%-------------------------------------------------------------------
-module(crossroad_helpers).
-author("wg").
-include("progress.hrl").
-include("model.hrl").
-include("constants.hrl").

%% API

-export([lanes_data_compare/2,
    empty_lane_data/0,
    get_row_crossroad/3,
    get_column_crossroad/3,
    get_cells_to_target/3,
    count_target_cell/3,
    get_begin_cell_number/4,
    get_target/3, get_ord_number/2]).


lanes_data_compare(ALanes = #lanes_data{in_no_lanes = AInLanes, out_no_lanes = AOutLanes},
    BLanes = #lanes_data{in_no_lanes = BInLanes, out_no_lanes = BOutLanes}) ->
    case (((AInLanes + AOutLanes) == (BInLanes + BOutLanes)) and ((BInLanes + BOutLanes) == 0)) of
        true ->
            {empty_lane_data(), 2};
        false ->
            case AInLanes + AOutLanes > BInLanes + BOutLanes of
                true ->
                    {ALanes, 0};
                false ->
                    {BLanes, 1}
            end
    end.


empty_lane_data() ->
    #lanes_data{in_no_lanes = 0, out_no_lanes = 0}.

get_row_helper(_, MaxPos, MaxPos, _) ->
    [];

get_row_helper(Indent, Pos, MaxPos, Change) ->
    ChildList = get_row_helper(Indent, Pos + Change, MaxPos, Change),
    [Indent + Pos] ++ ChildList.

get_row_crossroad(Y, #crossroad{
    width = Width
}, Orientation) ->
    case Orientation of
        rising ->
            get_row_helper(Y * Width, 0, Width, 1);
        falling ->
            get_row_helper(Y * Width, Width - 1, -1, -1)
    end.

get_column_crossroad(X, #crossroad{
    width = Width,
    length = Length
}, Orientation) ->
    case Orientation of
        rising ->
            get_row_helper(X, 0, Length * Width, Width);
        falling ->
            get_row_helper(X, (Length-1) * Width, -Width, -Width)
    end.



get_cells_to_target(Xstart, Xstart, _) ->
    [];

get_cells_to_target(Xstart, Xtarget, #crossroad{
    width = Width
}) ->
    {X1, Y1} = progress_ctx:id_to_position(Xstart, Width),
    {X2, Y2} = progress_ctx:id_to_position(Xtarget, Width),
    case Y1 == Y2 of
        true ->
            lists:foldl(fun(X, List) ->
                    [Y1 * Width + X] ++ List
                end, [], lists:seq(X1, X2));
        %%todo seq will fail when X1 > X2
        false ->
            case X1 == X2 of
            true ->
                lists:foldl(fun(Y, List) ->
                        [Y * Width + X1] ++ List
                    end, [], lists:seq(Y1, Y2));
            false ->
                erlang:error(not_implemented)
            end
    end.

get_in_edge_number(CellNo, #crossroad{
    width = Width,
    length = Length
}) ->
    {X1, Y1} = progress_ctx:id_to_position(CellNo, Width),
    case X1 + 1 of
        1 ->
            case Y1 of
                0 ->
                    0;
                _ ->
                    3
            end;
        Width ->
            case Y1 + 1 of
                Length ->
                    2;
                _ ->
                    1
            end;
        _ ->
            case Y1 + 1 of
                1 ->
                    0;
                Length ->
                    2
            end
    end.

get_out_edge_number(CellNo, #crossroad{
    width = Width,
    length = Length
}) ->
    {X1, Y1} = progress_ctx:id_to_position(CellNo, Width),
    case X1 + 1 of
        1 ->
            case Y1 + 1 of
                Length ->
                    2;
                _ ->
                    3
            end;
        Width ->
            case Y1 of
                0 ->
                    0;
                _ ->
                    1
            end;
        _ ->
            case Y1 + 1 of
                1 ->
                    0;
                Length ->
                    2
            end
    end.

count_target_cell(Rule, StartCellNo, Crossroad = #crossroad{
    width = Width,
    length = Length
})->
    {X1, Y1} = progress_ctx:id_to_position(StartCellNo, Width),
    case Rule of
        ?LEFT_RULE ->
            erlang:error(not_implemented);
        ?RIGHT_RULE ->
            StartCellNo;
        ?STRAIGHT_RULE ->
            case get_in_edge_number(StartCellNo, Crossroad) of
                3 ->
                    Y1 * Width + Width - 1;
                2 ->
                    X1 + (Length - 1) * Width;
                1 ->
                    Y1 * Width;
                0 ->
                    X1
            end
    end.


get_ord_number(RoadId, Crossroad) ->
    maps:fold(fun(N, Road, ParInfo) ->
        case Road == RoadId of
            true ->
                N;
            false ->
                ParInfo
        end
    end, init, Crossroad#crossroad.roads).

get_begin_cell_number(LaneId, #road_fraction{
    no_lanes = NoLanes
}, RoadId, Crossroad = #crossroad{
    width = Width,
    length = Length
}) ->
    %% this can fuck up based on lane id numeration
    case crossroad_helpers:get_ord_number(RoadId, Crossroad) of
        3 ->
            (Length - NoLanes + LaneId) * Width;
        0 ->
            (Width - NoLanes + LaneId) + (Length - 1) * Width;
        1 ->
            LaneId * Width;
        2 ->
            LaneId
    end.

get_middle(Side, Roads, #crossroad{
    roads = RoadIds,
    id = XRoadId
}) ->
    Road = maps:get(maps:get(Side, RoadIds), Roads),
    case Road#road.begin_crossroad of
        XRoadId ->
            (maps:get(0, Road#road.side_rising))#road_fraction.no_lanes;
        _ ->
            (maps:get(0, Road#road.side_falling))#road_fraction.no_lanes
    end.


get_target(CellId, Crossroad = #crossroad{
    width = Width,
    roads = Roads
}, RoadsData) ->
    {X1, Y1} = progress_ctx:id_to_position(CellId, Width),
    OutputEdge = get_out_edge_number(CellId, Crossroad),
    RoadId = maps:get(OutputEdge, Roads),
    case OutputEdge of
        3 ->
            LaneId = get_middle(0, RoadsData, Crossroad) - Y1;
        0 ->
            LaneId = get_middle(1, RoadsData, Crossroad) - X1;
        1 ->
            LaneId = Y1 - get_middle(2, RoadsData, Crossroad);
        2 ->
            LaneId = X1 - get_middle(3, RoadsData, Crossroad)
    end,
    {RoadId, LaneId}.
