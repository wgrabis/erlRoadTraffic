%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @doc
%%% This module is responsible for loading map from geoJSON, filtering
%%% it and converting to erlang map structure.
%%% @end
%%%-------------------------------------------------------------------
-module(mapLoader).
-author("Jakub Kudzia").

%% API
-export([load/1]).

%%%-------------------------------------------------------------------
%%% @doc
%%% Loads geoJSON from given FilePath
%%% @end
%%%-------------------------------------------------------------------
load(FilePath) ->
    {ok, BinaryJSON} = file:read_file(FilePath),
    JSON = jiffy:decode(BinaryJSON, [return_maps]),
    io:format("Loaded GEOJSON:~n~p", [JSON]),
    JSON.
