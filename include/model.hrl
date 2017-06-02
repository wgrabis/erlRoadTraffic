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
    roads = #{} :: #{id() => id()},
    length :: integer(),
    width :: integer(),
    empty_cells_before = undefined :: #{road_id() => #{lane_id() => non_neg_integer()}} | undefined
}).

-record(road, {
    id :: id(),
    side_rising = #{} :: #{fraction_id() => road_fraction()},
    side_falling = #{} :: #{fraction_id() => road_fraction()},
    begin_crossroad :: id(),
    end_crossroad :: id(),
    no_fractions :: integer(),
    empty_cells_before = undefined :: #{road_id() => #{lane_id() => non_neg_integer()}} | undefined
}).

-record(road_fraction, {
    id :: id(),
    velocity_limit :: non_neg_integer(),
    lanes = #{} :: #{id() => lane()},
    no_lanes :: integer(),
    special_rules =#{} :: map() %mapping for merging and splitting lanes
}).

%%Lanes are numerated from left to right
-record(lane, {
    id :: id(),
    cells = #{} :: #{id() => cell()},
    no_cells :: integer()
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
    node_data :: #{id() => #{}},
    transposed_graph :: #{id() => #{}}
}).


-type id()             :: non_neg_integer().
-type road_id()        :: id().
-type xroad_id()       :: id().
-type fraction_id()    :: id().
-type lane_id()        :: id().
-type cell_id()        :: id().
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
