-module(anyway_registration).

-export([init/2]).

-include("anyway_user.hrl").

init(Req0 = #{method := <<"POST">>}, State) ->
  {ok, Body, _} = cowboy_req:read_urlencoded_body(Req0),

  UserData = #user_registration_data{
    username = proplists:get_value(<<"username">>, Body),
    first_name = proplists:get_value(<<"first_name">>, Body),
    last_name = proplists:get_value(<<"last_name">>, Body),
    password = proplists:get_value(<<"password">>, Body)
  },

  try register_user(UserData) of
    Result -> handle_registration_result(Result, Req0, State)
  catch
    throw:{validation_error, Reason} ->
      Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => Reason}}),
      Res = json_response(400, Json, Req0),
      {ok, Res, State};
    throw:username_already_taken ->
      Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => <<"username already taken">>}}),
      Res = json_response(400, Json, Req0),
      {ok, Res, State}
  end;

init(Req0, State) ->
  Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => <<"method not allowed">>}}),
  Res = json_response(405, Json, Req0),
  {ok, Res, State}.

register_user(UserData) when is_record(UserData, user_registration_data) ->
  validate(UserData),
  anyway_users:register(UserData),
  ok.

handle_registration_result(ok, Req0, State) ->
  Json = jiffy:encode(<<"ok">>),
  Res = json_response(200, Json, Req0),
  {ok, Res, State};
handle_registration_result({error, Reason}, Req0, State) ->
  Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => Reason}}),
  Res = json_response(400, Json, Req0),
  {ok, Res, State}.

json_response(StatusCode, Json, Req) when is_integer(StatusCode) and is_binary(Json) ->
  cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/json">>}, Json, Req).

validate(#user_registration_data{
  username = Username,
  password = Password,
  first_name = FirstName,
  last_name = LastName
}) ->
  Fields = [
    {Username, username},
    {Password, password},
    {FirstName, first_name},
    {LastName, last_name}
  ],
  validate_required_fields(Fields).

validate_required_fields([{Value, FieldName} | Tail]) ->
  throw_if_missing(Value, FieldName),
  throw_if_empty(Value, FieldName),

  validate_required_fields(Tail);
validate_required_fields([]) ->
  ok.

throw_if_missing(undefined, FieldName) ->
  throw({validation_error, erlang:iolist_to_binary([io_lib:format("The field '~p' is required.", [FieldName])])});
throw_if_missing(_, _) ->
  ok.

throw_if_empty(<<"">> = Value, FieldName) when is_binary(Value) ->
  throw({validation_error, erlang:iolist_to_binary([io_lib:format("The field '~p' cannot be empty.", [FieldName])])});
throw_if_empty(Value, _FieldName) when is_binary(Value) ->
  ok.
