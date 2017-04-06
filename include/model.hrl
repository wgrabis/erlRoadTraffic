%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This file contains definitions of model data types.
%%% @end
%%%-------------------------------------------------------------------
-author("Jakub Kudzia").



-record(road_map, {
   roads = #{} :: #{id() => road()},
   crossroads = #{} :: #{id() => crossroad()}
}).

-record(crossroad, {
    id :: id(),
    cells = #{} :: #{id() => cell()},
    roads = #{} :: #{id() => id()}

}).

-record(road, {
    id :: id(),
    fractions = #{} :: #{id() => road_fraction()},
    begin_crossroad :: id(),
    end_crossroad :: id()
}).

-record(road_fraction, {
    id :: id(),
    velocity_limit :: non_neg_integer(),
    lanes = #{} :: #{id() => lane()}
}).

-record(lane, {
    id :: id(),
    cells = #{} :: #{id() => cell()}
}).

-record(cell, {
    id :: id(),
    car :: car()
}).

-record(car, {
    id :: id(),
    velocity :: integer()
}).

-type id()             :: non_neg_integer().
-type road_map()       :: #road_map{}.
-type crossroad()      :: #crossroad{}.
-type road()           :: #road{}.
-type road_fraction()  :: #road_fraction{}.
-type lane()           :: #lane{}.
-type cell()           :: #cell{}.
-type car()            :: #car{}.

-export_type([
    id/0, road_map/0, crossroad/0, road/0,
    road_fraction/0, lane/0, cell/0, car/0
]).
