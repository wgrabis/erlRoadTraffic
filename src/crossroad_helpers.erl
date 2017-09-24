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

%% API
-export([lanes_data_compare/2, empty_lane_data/0, get_row_crossroad/3,get_column_crossroad/3]).




lanes_data_compare(ALanes = #lanes_data{in_no_lanes = AInLanes, out_no_lanes = AOutLanes},
    BLanes = #lanes_data{in_no_lanes = BInLanes, out_no_lanes = BOutLanes}) ->
    case AInLanes + AOutLanes > BInLanes + BOutLanes of
        true ->
            {ALanes, 0};
        false ->
            {BLanes, 1}
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
            get_row_helper(X, Length * Width -1, -1, Width)
    end.



get_cells_to_target(Xstart, Xstart, _) ->
    [];

get_cells_to_target(Xstart, Xtarget, #crossroad{
    width = Width,
    length = Length
}) ->
    {X1, Y1} = progress_ctx:id_to_position(Xstart, Width),
    {X2, Y2} = progress_ctx:id_to_position(Xtarget, Width),
    case Y1 == Y2 of
        true ->
            lists:foldl(fun(X, List) ->
                    [Y1 * Width + X] ++ List
                end, [], lists:seq(X1, X2));
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

count_target_cell(Rule, StartCellNo, Crossroad)->
    erlang:error("Not impl").


get_ord_number(RoadId, Crossroad) ->
    .

get_begin_cell_number(LaneId, #road_fraction{
    no_lanes = NoLanes
}, RoadId, Crossroad = #crossroad{
    width = Width,
    length = Length
}) ->
    %% this can fuck up based on lane id numeration
    case crossroad_helpers:get_ord_number(RoadId, Crossroad) of
        0 ->
            (Length - NoLanes + LaneId) * Width;
        1 ->
            (Width - NoLanes + LaneId) + (Length - 1) * Width;
        2 ->
            LaneId * Width;
        3 ->
            LaneId
    end.