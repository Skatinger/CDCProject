-module(cdcproject_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, erlgraph, "index.html"}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 9999}], [
    {env, [{dispatch, Dispatch}]}
  ]),
  cdcproject_sup:start_link().


stop(_State) ->
  ok.
