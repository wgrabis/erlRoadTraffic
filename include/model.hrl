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
    side_rising = #{} :: #{id() => road_fraction()},
    side_falling = #{} :: #{id() => road_fraction()},
    begin_crossroad :: id(),
    end_crossroad :: id(),
    no_fractions :: integer()
}).

-record(road_fraction, {
    id :: id(),
    velocity_limit :: non_neg_integer(),
    lanes = #{} :: #{id() => lane()},
    no_lanes :: integer()
}).

-record(lane, {
    id :: id(),
    cells = #{} :: #{id() => cell()},
    no_cells :: integer(),
    length :: integer(),
    width :: integer()
}).

-record(cell, {
    id :: id(),
    car :: car() | undefined
}).

-record(car, {
    id :: id(),
    velocity :: integer()
}).

-record(edge, {
    way_id :: id(),
    node :: id()
}).

-record(graphData, {
    graph :: #{id() => list()},
    x_graph :: #{id() => list()},
    way_data :: #{id() => #{}},
    node_data :: #{id() => #{}}
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
