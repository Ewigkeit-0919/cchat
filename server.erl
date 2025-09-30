-module(server).
-export([start/1, stop/1]).

-record(server_st, {
  name,
  channels
}).

-record(channel_st, {
  name,
  clients
}).

initial_server_state(ServerAtom) ->
  #server_st{
    name = ServerAtom,
    channels = []
  }.

initial_channel_state(ChannelAtom) ->
  #channel_st{
    name = ChannelAtom,
    clients = []
  }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  genserver:start(ServerAtom, initial_server_state(ServerAtom), fun handleS/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  genserver:request(ServerAtom, {stop}),
  genserver:stop(ServerAtom).

handleS(St, {stop}) ->
  lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, St#server_st.channels),
  {reply, ok, St};



handleS(St, {join, Channel, Pid}) ->
  case whereis(list_to_atom(Channel)) of
    undefined -> % true if no such channel
      genserver:start(list_to_atom(Channel), initial_channel_state(Channel), fun handleC/2);
    _ -> % channel exists
      ok
  end,

  case catch (genserver:request(list_to_atom(Channel), {join, Pid})) of
    ok -> % could join
      {reply, ok, St#server_st{
        channels = [Channel | St#server_st.channels]}};
    _ ->
      {reply, error, St}
  end.

handleC(St, {join, Pid}) ->
  case lists:member(Pid, St#channel_st.clients) of
    false -> % user not found
      {reply, ok, St#channel_st{clients = [Pid | St#channel_st.clients]}}; % add user
    {'EXIT', _} -> % user is already in channel
      {reply, error, St}
  end;

handleC(St, {leave, Pid}) ->
  case lists:member(Pid, St#channel_st.clients) of
    true -> % user found
      {reply, ok, St#channel_st{clients = lists:delete(Pid, St#channel_st.clients)}}; %delete user
    _ -> % no user found
      {reply, error, St}
  end;


handleC(St, {message_send, Pid, Msg, Sender}) ->
  case lists:member(Sender, St#channel_st.clients) of
    true -> % user in channel
      spawn(fun() ->
        [genserver:request(ChannelUsers, {message_receive, St#channel_st.name, Pid, Msg}) || ChannelUsers <- St#channel_st.clients, ChannelUsers =/= Sender]
            end),
      {reply, message_send, St};
    false -> % user not in channel
      {reply, error, St}
  end.
