%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @doc
%%% This module is responsible for loading map from geoJSON, filtering
%%% it and converting to erlang map structure.
%%% @end
%%%-------------------------------------------------------------------
-module(map_loader).
-author("Jakub Kudzia").

%% API
-export([load/1, split_to_nodes_and_ways/1, build_map/1]).


%%%-------------------------------------------------------------------
%%% @doc
%%% Loads json from given FilePath
%%% @end
%%%-------------------------------------------------------------------
load(FilePath) ->
    {ok, BinaryJson} = file:read_file(FilePath),
    jiffy:decode(BinaryJson, [return_maps]).


%%%-------------------------------------------------------------------
%%% @doc
%%% Splits input map to 2 maps. First one stores nodes, second ways.
%%% In both maps keys are nodes/ways id.
%%% @end
%%%-------------------------------------------------------------------
split_to_nodes_and_ways(Map) ->
    lists:foldl(fun(Element, {Nodes, Ways}) ->
        NewKey = maps:get(<<"id">>, Element),
        case maps:get(<<"type">>, Element) of
            <<"node">> ->
                {Nodes#{NewKey => Element}, Ways};
            <<"way">> ->
                {Nodes, Ways#{NewKey => Element}}
        end
    end, {#{}, #{}}, maps:get(<<"elements">>, Map)).


%%%-------------------------------------------------------------------
%%% @doc
%%% Builds matrix of adjacency for each node.
%%% @end
%%%-------------------------------------------------------------------
build_map(Ways) ->
    lists:foldl(fun(WayId, AccIn) ->
        WayDescription = maps:get(WayId, Ways),
        add_way(WayDescription, AccIn)
    end, #{}, maps:keys(Ways)).


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Adds end Nodes of given Way to adjacency map Map.
%%% @end
%%%-------------------------------------------------------------------
add_way(WayDescription, Map) ->
    Nodes = maps:get(<<"nodes">>, WayDescription),
    Node1 = hd(Nodes),
    Node2 = lists:last(Nodes),
    Acc = update_node(Node1, Node2, Map),
    update_node(Node2, Node1, Acc).


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Updates entry in adjacency matrix for given Node1.
%%% Node2 is added to list of Node1's neighbours list in Map.
%%% @end
%%%-------------------------------------------------------------------
update_node(Node1, Node2, Map) ->
    maps:update_with(Node1, fun(Nodes) ->
        [Node2 | Nodes]
    end, [Node2], Map).