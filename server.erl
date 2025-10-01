-module(server).
-export([start/1, stop/1]).

% This record defines the structure of the state of a server
-record(server_st, {
	name,      % atom of the server process
	channels,  % list of channels
	nicks      % list of all user nicknames
}).

% This record defines the structure of the state of a channel
-record(channel_st, {
	name,      % atom of the channel process
	clients    % list of client PIDs in this channel
}).

% Initialize server state
initial_server_state(ServerAtom) ->
	#server_st{
		name = ServerAtom,
		channels = [],
		nicks = []
	}.

% Initialize channel state
initial_channel_state(ChannelAtom) ->
	#channel_st{
		name = ChannelAtom,
		clients = []
	}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
	genserver:start(ServerAtom, initial_server_state(ServerAtom), fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
	genserver:request(ServerAtom, {stop}),
	genserver:stop(ServerAtom).

% Stop the server and all associated channel processes
handle(St, {stop}) ->
	lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, St#server_st.channels),
	{reply, ok, St};

% Handle a user joining a channel - creates channel if it doesn't exist
handle(St, {join, Channel, Pid}) ->
	case whereis(list_to_atom(Channel)) of
		undefined -> % true if no such channel
			genserver:start(list_to_atom(Channel), initial_channel_state(Channel), fun handleChannel/2);
		_ -> % channel exists
			ok
	end,

	case catch (genserver:request(list_to_atom(Channel), {join, Pid})) of
		ok -> % could join
			{reply, ok, St#server_st{channels = [Channel | St#server_st.channels]}};
		_ ->
			{reply, error, St}
	end;

% Handle nickname change request - checks if new nick is available
handle(St, {nick, OldNick, NewNick}) ->
	% Extract the existing nicks list from server state
	ExistingNicks = St#server_st.nicks,

	% First, ensure the old nick is in the list
	ExistingNicksWithOld = case lists:member(OldNick, ExistingNicks) of
							   true -> ExistingNicks;
							   false -> [OldNick | ExistingNicks]
						   end,

	% Check if the new nickname already exists
	case lists:member(NewNick, ExistingNicksWithOld) of
		true ->
			{reply, error, St};
		false ->
			% Update state: remove old nick and add new nick
			NewState = St#server_st{nicks = [NewNick | lists:delete(OldNick, ExistingNicksWithOld)]},
			{reply, ok, NewState}
	end.

% Add a user to the channel's client list
handleChannel(St, {join, Pid}) ->
	case lists:member(Pid, St#channel_st.clients) of
		false -> % user not found
			{reply, ok, St#channel_st{clients = [Pid | St#channel_st.clients]}}; % add user
		{'EXIT', _} -> % user is already in channel
			{reply, error, St}
	end;

% Remove a user from the channel's client list
handleChannel(St, {leave, Pid}) ->
	case lists:member(Pid, St#channel_st.clients) of
		true -> % user found
			{reply, ok, St#channel_st{clients = lists:delete(Pid, St#channel_st.clients)}}; %delete user
		_ -> % no user found
			{reply, error, St}
	end;

% Broadcast a message to all users in the channel except the sender
handleChannel(St, {message_send, Pid, Msg, Sender}) ->
	case lists:member(Sender, St#channel_st.clients) of
		true -> % user in channel
			spawn(fun() ->
				[genserver:request(ChannelUsers, {message_receive, St#channel_st.name, Pid, Msg}) || ChannelUsers <- St#channel_st.clients, ChannelUsers =/= Sender]
				  end),
			{reply, ok, St};
		false -> % user not in channel
			{reply, error, St}
	end.
