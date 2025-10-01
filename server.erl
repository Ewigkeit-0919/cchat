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
	genserver:request(ServerAtom, {stop}), % stop all the channels
	genserver:stop(ServerAtom). % stop the server

% Stop all the associated channel processes
handle(St, {stop}) ->
	lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, St#server_st.channels),
	{reply, ok, St};

% Handle a user joining a channel
% Registers the user's nickname if it's their first join (for nick_taken check)
% Server first creates the channel (if it doesn't exist), then channel adds the client
handle(St, {join, Client, Channel, Nick}) ->
	% 1 Check if this nick is already registered
	ExistingNicks = St#server_st.nicks,
	NickRegistered = lists:member(Nick, ExistingNicks),
	% Register nick if it's the first time
	UpdatedNicks = case NickRegistered of
					   true -> ExistingNicks;
					   false -> [Nick | ExistingNicks]
				   end,
	% 2 Check if channel exists, create if needed
	case whereis(list_to_atom(Channel)) of
		undefined -> % channel doesn't exist
			genserver:start(list_to_atom(Channel), initial_channel_state(Channel), fun handleChannel/2);
		_ -> % channel exists
			ok
	end,
	% 3 Join the channel
	case catch (genserver:request(list_to_atom(Channel), {join, Client})) of
		ok -> % successfully joined
			{reply, ok, St#server_st{channels = [Channel | St#server_st.channels], nicks = UpdatedNicks}};
		_ ->
			% Join failed, but still update nicks if it was a new nick
			{reply, error, St#server_st{nicks = UpdatedNicks}}
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
handleChannel(St, {join, Client}) ->
	case lists:member(Client, St#channel_st.clients) of
		false -> % user not found, then add
			{reply, ok, St#channel_st{clients = [Client | St#channel_st.clients]}}; % add user
		true -> % user is already in channel
			{reply, error, St}
	end;

% Remove a user from the channel's client list
handleChannel(St, {leave, Client}) ->
	case lists:member(Client, St#channel_st.clients) of
		true -> % user found, then delete
			{reply, ok, St#channel_st{clients = lists:delete(Client, St#channel_st.clients)}}; %delete user
		false -> % user not found
			{reply, error, St}
	end;

% Broadcast a message to all users in the channel except the sender
handleChannel(St, {message_send, Sender, Nick, Msg}) ->
	case lists:member(Sender, St#channel_st.clients) of
		true -> % user in channel, send messages to all the users in the channel
			spawn(fun() ->
				[genserver:request(ChannelUsers, {message_receive, St#channel_st.name, Nick, Msg}) || ChannelUsers <- St#channel_st.clients, ChannelUsers =/= Sender]
				  end),
			{reply, ok, St};
		false -> % user not in channel
			{reply, error, St}
	end.
