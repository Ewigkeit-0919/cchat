-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
	gui, % atom of the GUI process
	nick, % nick/username of the client
	server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
	#client_st{
		gui = GUIAtom,
		nick = Nick,
		server = ServerAtom
	}.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
	% Attempt to join the user to the specified channel along with nick by sending a request to the server
	% Server first creates the channel, then channel adds the client
	case catch (genserver:request(St#client_st.server, {join, self(), Channel, St#client_st.nick})) of
		% If the server process crashes or is unreachable, catch the EXIT signal
		{'EXIT', _} ->
			{reply, {error, server_not_reached, "failed to reach the server"}, St};
		% If the server timeout occurs
		timeout_error ->
			{reply, {error, server_not_reached, "failed to reach the server (timeout)"}, St};
		% If the server responds with 'ok', the join operation was successful
		ok ->
			{reply, ok, St};
		% If the server responds with 'error', the user is already in the channel
		error ->
			{reply, {error, user_already_joined, "user already joined the channel"}, St}
	end;

% Leave channel
handle(St, {leave, Channel}) ->
	% Send a leave request directly to the channel process
	% Convert the channel name from string to atom to get the process identifier
	% Self() is the Pid of this client process
	case catch (genserver:request(list_to_atom(Channel), {leave, self()})) of
		{'EXIT', _} ->
			{reply, {error, server_not_reached, "failed to reach the server"}, St};
		timeout_error ->
			{reply, {error, server_not_reached, "failed to reach the server (timeout)"}, St};
		ok ->
			{reply, ok, St};
		error ->
			{reply, {error, user_not_joined, "user not in the channel"}, St}
	end;

% Sending message (from GUI, to channel)
% GUI -> Client process -> Channel process -> Other clients in channel
handle(St, {message_send, Channel, Msg}) ->
	% Send a message to the specified channel
	% The request includes the sender's nickname, message content, and PID
	case (catch genserver:request(list_to_atom(Channel), {message_send, self(), St#client_st.nick, Msg})) of
		{'EXIT', _} ->
			{reply, {error, server_not_reached, "failed to reach the server"}, St};
		timeout_error ->
			{reply, {error, server_not_reached, "failed to reach the server (timeout)"}, St};
		ok ->
			{reply, ok, St};
		error ->
			{reply, {error, user_not_joined, "user not in channel"}, St}
	end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
	% Send a request to the server to change the nickname
	% Include both the old nick and the new nick for validation
	case catch (genserver:request(St#client_st.server, {nick, St#client_st.nick, NewNick})) of
		{'EXIT', _} ->
			{reply, {error, server_not_reached, "failed to reach the server"}, St};
		timeout_error ->
			{reply, {error, server_not_reached, "failed to reach the server (timeout)"}, St};
		ok ->
			{reply, ok, St#client_st{nick = NewNick}};
		error ->
			{reply, {error, nick_taken, "Nick is taken"}, St}
	end;
% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
	{reply, St#client_st.nick, St};

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
	gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
	{reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
	% Any cleanup should happen here, but this is optional
	{reply, ok, St};

% Catch-all for any unhandled requests
handle(St, Data) ->
	{reply, {error, not_implemented, "Client does not handle this command"}, St}.
