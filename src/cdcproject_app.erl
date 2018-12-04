-module(cdcproject_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

  io:format("Starting Webserver...~n"),

%%  % start cowboy server
%%  Dispatch = cowboy_router:compile([
%%    {'_', [
%%      % routes: index, websocket and sources (js), emtpy atm
%%      {"/", cowboy_static, {priv_file, websocket, "index.html"}},
%%      {"/websocket", ws_h, []},
%%      {"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}}
%%    ]}
%%  ]),
%%  {ok, _} = cowboy:start_http(http, [{port, 8080}], #{
%%    env => #{dispatch => Dispatch}
%%  }),
%%  % start websocket supervisor
%%  websocket_sup:start_link(),

  % start simulation app
  io:format("Starting Simulation...~n"),
  cdcproject_sup:start_link().

stop(_State) ->
  ok.

