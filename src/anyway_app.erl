-module(anyway_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/register", anyway_registration, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 7778}], #{env => #{dispatch => Dispatch}}),

  anyway_mnesia:start(),

  anyway_sup:start_link().

stop(_State) ->
  ok.
