%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------

%% Constants
-define(CAR_SIZE, 5).
-define(DEAD_END, none_xroad).
-define(DEFAULT_LANES_NUM, <<"1">>).
-define(DEFAULT_SPEED_LIMIT, <<"5">>).
-define(DEFAULT_ACCELERATION, 1).
-define(CHANGE_LANE_PROBABILITY, 0.2).
-define(LAT_LENGTH, 11.1132).
-define(LON_LENGTH, 7.8847).
-define(DEFAULT_MAXSPEED, "50").
-define(DEAD_END_CROSSROAD_SIZE, 1).
-define(LEFT_RULE, 0).
-define(STRAIGHT_RULE, 1).
-define(RIGHT_RULE, 2).
-define(MERGE_LEFT, 3).
-define(MERGE_RIGHT, 4).