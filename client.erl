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


% Join channel
% Checks if the server exists and if it does it tries to send its pid and nick to the server.
handle(St, {join, Channel}) ->
  ServerExists = lists:member(St#client_st.server, registered()),
  if ServerExists ->
    Result = (catch (genserver:request(St#client_st.server, {join, Channel, St#client_st.nick, self()}))),
    case Result of
      {error, user_already_joined,_} -> {reply, Result, St};
      join -> {reply, ok, St};
      {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
    end;
    true ->
      {reply, {error, server_not_reached, "Server unreachable"}, St}
  end;

% Leave channel
% Sends a leave request to the server and checks if the server processed it correctly.
handle(St, {leave, Channel}) ->
  Result = (catch genserver:request(St#client_st.server, {leave, Channel, self()})),
  case Result of
    {error, user_not_joined, _} -> {reply, Result, St};
    leave -> {reply, ok, St};
    {'EXIT', _} -> {reply, ok, St}
  end;

% Sending message (from GUI, to server)
% Tries to send a message to the server which will distribute it to the channel
handle(St = #client_st {nick = Nick}, {message_send, Channel, Msg}) ->
  Result = (catch (genserver:request(St#client_st.server, {message_send, Channel, Nick, self(), Msg}))),
  case Result of
    {error, user_not_joined, _} -> {reply, Result, St};
    ok -> {reply, ok , St};
    {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
  end;

% Change nick (no check, local only)
% Tries to change the nick and errors out on a taken nick or on server disconnect
handle(St, {nick, NewNick}) ->
  Reply = (catch (genserver:request(St#client_st.server, {nick, St#client_st.nick, NewNick}))),
  case Reply of
    error -> {reply, {error, nick_taken, "Nick is taken"}, St};
    nick -> {reply, ok, St#client_st{nick = NewNick}};
    {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
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
