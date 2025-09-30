-module(server).
-export([start/1,stop/1]).

% Server record that holds the channels and the nicks
% channels now stores {ChannelName, [UserPids]} tuples
-record(server_st, {
  channels = [],
  nicks = []
}).


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  genserver:start(ServerAtom, #server_st{}, fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  genserver:stop(ServerAtom).

% Handle join request
% Sees if the channel that the client attempts to join exists, if it does it joins that channel
% If the channel does not exist it creates the channel and then joins the client to it
handle(State = #server_st{channels = ExistingChannels, nicks = ExistingNicks}, {join, ChannelToJoin, NickToJoin, Sender}) ->
  case lists:keyfind(ChannelToJoin, 1, ExistingChannels) of
    {ChannelToJoin, Users} ->
      % Channel exists, check if user already joined
      IsAlreadyInChannel = lists:member(Sender, Users),
      if IsAlreadyInChannel ->
        {reply, {error, user_already_joined, "User already in chat"}, State};
        true ->
          % Add user to channel
          UpdatedChannels = lists:keyreplace(ChannelToJoin, 1, ExistingChannels, {ChannelToJoin, [Sender | Users]}),
          % Check and add nick if needed
          NickExists = lists:member(NickToJoin, ExistingNicks),
          if NickExists ->
            {reply, join, State#server_st{channels = UpdatedChannels}};
            true ->
              {reply, join, State#server_st{channels = UpdatedChannels, nicks = [NickToJoin | ExistingNicks]}}
          end
      end;
    false ->
      % Channel does not exist, create it with this user
      NewChannels = [{ChannelToJoin, [Sender]} | ExistingChannels],
      NickExists = lists:member(NickToJoin, ExistingNicks),
      if NickExists ->
        {reply, join, State#server_st{channels = NewChannels}};
        true ->
          {reply, join, State#server_st{channels = NewChannels, nicks = [NickToJoin | ExistingNicks]}}
      end
  end;

% Handle leave request
% It checks if the channel exists and if it does it checks if the client is in the channel
% If the channel doesn't exist or the client was not in the channel it errors out
handle(State = #server_st{channels = ExistingChannels}, {leave, ChannelToLeave, PidToLeave}) ->
  case lists:keyfind(ChannelToLeave, 1, ExistingChannels) of
    {ChannelToLeave, Users} ->
      IsInChannel = lists:member(PidToLeave, Users),
      if IsInChannel ->
        ResultingList = lists:delete(PidToLeave, Users),
        UpdatedChannels = lists:keyreplace(ChannelToLeave, 1, ExistingChannels, {ChannelToLeave, ResultingList}),
        {reply, leave, State#server_st{channels = UpdatedChannels}};
        true ->
          {reply, {error, user_not_joined, "User did not join"}, State}
      end;
    false ->
      {reply, {error, user_not_joined, "User did not join"}, State}
  end;

% Handle message send
% Distributes messages to all connected clients in the channel
handle(State = #server_st{channels = ExistingChannels}, {message_send, Channel, SenderNick, SenderPid, Msg}) ->
  case lists:keyfind(Channel, 1, ExistingChannels) of
    {Channel, Users} ->
      case lists:member(SenderPid, Users) of
        true ->
          spawn(fun() ->
            lists:foreach(
              fun(Pid) ->
                if Pid == SenderPid -> skip;
                  true -> genserver:request(Pid, {message_receive, Channel, SenderNick, Msg})
                end
              end,
              Users)
                end),
          {reply, ok, State};
        false ->
          {reply, {error, user_not_joined, "User is not in channel"}, State}
      end;
    false ->
      {reply, {error, user_not_joined, "User is not in channel"}, State}
  end;

% Re-nick
% Looks to see if the nick is taken, if it is then it errors out, otherwise it returns an updated state
handle(State = #server_st{nicks = ExistingNicks}, {nick, OldNick, NewNick}) ->
  NickExists = lists:member(NewNick, ExistingNicks),
  if NickExists ->
    {reply, error, State};
    true ->
      NewState = #server_st {channels = State#server_st.channels, nicks = [NewNick | lists:delete(OldNick, ExistingNicks)]},
      {reply, nick, NewState}
  end;

% Catch All >:(
handle(State, Data) ->
  {error, instruction_not_found, ["The instruction does not exist", State, Data]}.