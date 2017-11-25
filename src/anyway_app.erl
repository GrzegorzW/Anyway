-module(anyway_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->

  debugger:start(),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/users", anyway_registration, []},
      {"/users/:username", anyway_user_details, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 7778}], #{env => #{dispatch => Dispatch}}),

  anyway_mnesia:start(),

  anyway_sup:start_link().

stop(_State) ->
  ok.