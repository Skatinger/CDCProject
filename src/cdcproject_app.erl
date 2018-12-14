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
      {"/", cowboy_static, {priv_file, cdcproject, "index.html"}},
      {"/websocket", ws_h, []},
      {"/static/[...]", cowboy_static, {priv_dir, cdcproject, "static"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  }),
  cdcproject_sup:start_link().


stop(_State) ->
  ok.
