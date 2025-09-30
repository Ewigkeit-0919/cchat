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
  % TODO: Implement this function
  % {reply, ok, St} ;
  % 向服务器发送加入频道请求
  % 使用 catch 捕获异常（如服务器不存在或无响应）
  case catch (genserver:request(St#client_st.server, {join, Channel, self()})) of
    {'EXIT', _} ->
      % 服务器无法访问（不存在或崩溃）
      {reply, {error, server_not_reached, "Server unreachable"}, St};
    ok ->
      % 加入成功
      {reply, ok, St};
    user_already_joined ->
      % 已经在该频道中
      {reply, {error, user_already_joined, "User already joined this channel"}, St};
    _ ->
      % 其他错误
      {reply, {error, server_not_reached, "Unexpected error"}, St}
  end;

% Leave channel
handle(St, {leave, Channel}) ->
  % TODO: Implement this function
  % {reply, ok, St} ;
  % 注意：这里应该向服务器发送请求，而不是直接向频道发送
  % 修改为通过服务器处理，保持架构一致性
  case catch (genserver:request(St#client_st.server, {leave, Channel, self()})) of
    {'EXIT', _} ->
      % 服务器无法访问
      {reply, {error, server_not_reached, "Server unreachable"}, St};
    ok ->
      % 离开成功
      {reply, ok, St};
    user_not_joined ->
      % 用户不在该频道中
      {reply, {error, user_not_joined, "User is not in this channel"}, St};
    _ ->
      % 其他错误
      {reply, {error, server_not_reached, "Unexpected error"}, St}
  end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
  % TODO: Implement this function
  % {reply, ok, St} ;
  % 向服务器发送消息请求
  % 修改为通过服务器转发，而不是直接发给频道
  case catch (genserver:request(St#client_st.server,
    {message_send, Channel, St#client_st.nick, Msg, self()})) of
    {'EXIT', _} ->
      % 服务器无法访问
      {reply, {error, server_not_reached, "Server unreachable"}, St};
    ok ->
      % 消息发送成功
      {reply, ok, St};
    user_not_joined ->
      % 用户不在该频道中，无法发送消息
      {reply, {error, user_not_joined, "User is not in this channel"}, St};
    _ ->
      % 其他错误
      {reply, {error, server_not_reached, "Unexpected error"}, St}
  end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
  {reply, ok, St#client_st{nick = NewNick}};

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
