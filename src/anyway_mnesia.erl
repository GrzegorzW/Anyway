-module(anyway_mnesia).

-export([start/0]).

-include("anyway_user.hrl").

start() ->
  mnesia:create_schema([node()]),
  mnesia:start(),

  ensure_mnesia_running(),
  ensure_mnesia_dir(),

  try
    mnesia:table_info(user, type)
  catch
    exit: _ ->
      {atomic, ok} = mnesia:create_table(user, [{attributes, record_info(fields, user)},
        {disc_copies, [node()]}])
  end.

ensure_mnesia_running() ->
  case mnesia:system_info(is_running) of
    yes ->
      ok;
    starting ->
      timer:sleep(1000),
      ensure_mnesia_running();
    Reason when Reason =:= no; Reason =:= stopping ->
      throw({error, mnesia_not_running})
  end.

ensure_mnesia_dir() ->
  MnesiaDir = mnesia:system_info(directory) ++ "/",
  case filelib:ensure_dir(MnesiaDir) of
    {error, Reason} ->
      throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
    ok ->
      ok
  end.
