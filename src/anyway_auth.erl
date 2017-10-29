-module(anyway_auth).

-export([register_user/1]).

-include("anyway_user.hrl").

generate_random_salt() ->
  base64:encode(crypto:strong_rand_bytes(128)).

hash_password(Subject, Salt) ->
  <<Digest:512/big-unsigned-integer>> = crypto:hash(sha512, erlang:iolist_to_binary([Salt, Subject])),
  lists:flatten(io_lib:format("~128.16.0b", [Digest])).

create_user(UserData) when is_record(UserData, user_registration_data) ->
  Salt = generate_random_salt(),

  #user{
    username = UserData#user_registration_data.username,
    first_name = UserData#user_registration_data.first_name,
    last_name = UserData#user_registration_data.last_name,
    password = hash_password(UserData#user_registration_data.password, Salt),
    salt = Salt,
    token = hash_password(UserData#user_registration_data.username, Salt)
  }.

register_user(UserData) when is_record(UserData, user_registration_data) ->
  User = create_user(UserData),
  anyway_users:insert(User),
  erlang:iolist_to_binary([User#user.token]).
