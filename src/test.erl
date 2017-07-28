%%%-------------------------------------------------------------------
%%% @author wg
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2017 1:38 PM
%%%-------------------------------------------------------------------
-module(test).
-author("wg").

%% API
-export([test/0, test2/1]).


test()->
  Data = map_loader:load("/home/wg/Documents/erlRoadTraffic/test/test5.json"),
  {Nodes, Ways} = map_loader:split_to_nodes_and_ways(Data),
  model:initialize(Nodes, Ways).
%%  model:build_graphs(Ways).


test2(TestFile) ->
  Data = map_loader:load(TestFile),
  {Nodes, Ways} = map_loader:split_to_nodes_and_ways(Data),
  model:initialize(Nodes, Ways).