-module(anyway_users).

-export([
  register/1,
  get_by_username/1
]).

-include("anyway_user.hrl").

register(UserData) when is_record(UserData, user_registration_data) ->
  register_jid(
    UserData#user_registration_data.username,
    UserData#user_registration_data.password
  ),
  User = create_user(UserData),
  insert(User),
  ok.

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

create_user(UserData) when is_record(UserData, user_registration_data) ->
  #user{
    username = UserData#user_registration_data.username,
    first_name = UserData#user_registration_data.first_name,
    last_name = UserData#user_registration_data.last_name
  }.

register_jid(Username, Password) ->
  {ok, _} = application:ensure_all_started(gun),
  {ok, _} = application:ensure_all_started(anyway),
  {ok, ConnPid} = gun:open("ejabberd", 5280, #{transport=>ssl}),
  {ok, _} = gun:await_up(ConnPid),

  {ok, Host} = application:get_env(anyway, ejabberd_domain),
  {ok, Auth} = application:get_env(anyway, ejabberd_auth),

  erlang:display(Host),
  erlang:display(Auth),

  Json = jiffy:encode(#{
    <<"user">> => Username,
    <<"password">> => Password,
    <<"host">> => erlang:iolist_to_binary([Host])
  }),

  MRef = gun:post(ConnPid, "/api/register", [
    {<<"content-type">>, "application/json"},
    {<<"authorization">>, erlang:iolist_to_binary(["Basic ", base64:encode(Auth)])}
  ], Json),

  receive
    {gun_response, _ConnPid, _StreamRef, fin, _Status, _Headers} ->
      no_data;
    {gun_response, ConnPid, StreamRef, nofin, Status, _Headers} ->
      receive_data(ConnPid, MRef, StreamRef, Status);
    {'DOWN', MRef, process, _ConnPid, Reason} ->
      exit(Reason)
  after 1000 ->
    exit(timeout)
  end.

receive_data(ConnPid, MRef, StreamRef, Status) ->
  receive
    {gun_data, ConnPid, StreamRef, nofin, Data} ->
      io:format("nofin: ~s~n", [Data]);
    {gun_data, ConnPid, _StreamRef, fin, Data} ->
      handle_data(Status, jiffy:decode(Data, [return_maps])),
      io:format("fin: ~s~n", [Data]);
    {'DOWN', MRef, process, ConnPid, Reason} ->
      error_logger:error_msg("Oops!"),
      exit(Reason)
  after 1000 ->
    exit(timeout)
  end.

handle_data(200, Data) ->
  {ok, Data};
handle_data(_Status, Data) ->
  handle_error(Data).

handle_error(_Data = #{message := Message}) ->
  throw({error, Message});

handle_error(Data) ->
  erlang:display(?MODULE),
  erlang:display(?FUNCTION_NAME),
  erlang:display(Data).
