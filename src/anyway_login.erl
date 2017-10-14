-module(anyway_login).

-export([init/2]).

init(Req0 = #{method := <<"POST">>}, State) ->
  {ok, Body, _} = cowboy_req:read_urlencoded_body(Req0),

  Username = proplists:get_value(<<"username">>, Body),
  Password = proplists:get_value(<<"password">>, Body),

  try anyway_auth:login(Username, Password) of
    Token ->
      Json = jiffy:encode(#{<<"success">> => #{<<"token">> => Token}}),
      json_response(200, Json, Req0)
  catch
    error:badmatch ->
      Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => <<"login failed">>}}),
      Res = json_response(400, Json, Req0),
      {ok, Res, State};
    _:Reason ->
      erlang:display(Reason),

      Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => <<"login failed">>}}),
      Res = json_response(400, Json, Req0),
      {ok, Res, State}
  end;
init(Req0, State) ->
  Json = jiffy:encode(#{<<"error">> => #{<<"reason">> => <<"method not allowed">>}}),
  Res = json_response(405, Json, Req0),
  {ok, Res, State}.

json_response(StatusCode, Json, Req) when is_integer(StatusCode) and is_binary(Json) ->
  cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/json">>}, Json, Req).
