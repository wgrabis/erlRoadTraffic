%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @doc
%%% This module is responsible for loading map from JSON, filtering
%%% it and creating erlang maps out of it.
%%% @end
%%%-------------------------------------------------------------------
-module(map_loader).
-author("Jakub Kudzia").

%% API
-export([load/1, split_to_nodes_and_ways/1]).

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
