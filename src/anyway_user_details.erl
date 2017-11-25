-module(anyway_user_details).

-export([init/2]).

-include("anyway_user.hrl").

init(Req0 = #{method := <<"GET">>}, State) ->
  Username = cowboy_req:binding(username, Req0),
  erlang:display(Username),
  Result = anyway_users:get_by_username(Username),
  handle_result(Result, Req0, State);

init(Req0, State) ->
  Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => <<"method not allowed">>}}),
  Res = json_response(405, Json, Req0),
  {ok, Res, State}.

handle_result(user_not_found, Req, State) ->
  Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => <<"user not found">>}}),
  Res = json_response(404, Json, Req),
  {ok, Res, State};

handle_result(User, Req, State) when is_record(User, user) ->
  Json = jiffy:encode(
    #{<<"ok">> => #{
      <<"firs_tname">> => User#user.first_name,
      <<"last_name">> => User#user.last_name,
      <<"username">> => User#user.username
    }}),
  Res = json_response(200, Json, Req),
  {ok, Res, State}.

json_response(StatusCode, Json, Req) when is_integer(StatusCode) and is_binary(Json) ->
  cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, Json, Req).
