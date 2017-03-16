%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @doc
%%% erlRoadTraffic public API
%%% @end
%%%-------------------------------------------------------------------
-module(erlRoadTraffic_app).
-author("Jakub Kudzia").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    erlRoadTraffic_sup:start_link(),
    mapLoader:load(_StartArgs).


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
