-module(anyway_users).

-export([insert/1, get_token_by_username/1, find_by_username/1, get_by_username/1]).

-include("anyway_user.hrl").

insert(User) when is_record(User, user) ->
  mnesia:wait_for_tables([user], 1000),

  throw_if_username_already_taken(User),

  Fun = fun() ->
    mnesia:write(User)
        end,
  {atomic, ok} = mnesia:transaction(Fun).

throw_if_username_already_taken(#user{username = Username}) ->
  CurrentUsers = find_by_username(Username),
  throw_if_user_list_not_empty(CurrentUsers).

throw_if_user_list_not_empty([]) ->
  ok;
throw_if_user_list_not_empty(_List) ->
  throw(username_already_taken).

find_by_username(Username) ->
  Fun = fun() ->
    mnesia:read(user, Username)
        end,
  {atomic, Result} = mnesia:transaction(Fun),
  Result.

get_by_username(Username) ->
  case find_by_username(Username) of
    [] -> user_not_found;
    [User] -> User
  end.

get_token_by_username(Username) ->
  [{user, _Username, _Password, _Salt, Token}] = find_by_username(Username),
  erlang:iolist_to_binary([Token]).
