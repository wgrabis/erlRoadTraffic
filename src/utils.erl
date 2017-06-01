%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(utils).
-author("Jakub Kudzia").

%% API
-export([recursive_get/2]).

recursive_get([], Terms) ->
    {ok, Terms};

recursive_get([Key | Keys], Terms) ->
    case maps:find(Key, Terms) of
        {ok, NewTerms} -> recursive_get(Keys, NewTerms);
        error -> {error, not_found, {Key, Terms}}
    end;

recursive_get(Key, Terms) ->
    recursive_get([Key], Terms).