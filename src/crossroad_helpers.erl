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