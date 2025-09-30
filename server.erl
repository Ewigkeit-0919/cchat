-module(server).
-export([start/1, stop/1]).

% This record defines the structure of the state of a server
% name: 服务器注册的名称
% channels: 已创建的频道名称列表（字符串形式）
-record(server_st, {
  name,
  channels
}).

% This record defines the structure of the state of a channel
% name: 频道名称（字符串形式，如 "#chat"）
% clients: 加入该频道的客户端进程 PID 列表
-record(channel_st, {
  name,
  clients
}).

% 初始化服务器状态
initial_server(ServerAtom) ->
  #server_st{
    name = ServerAtom,
    channels = []  % 初始没有任何频道
  }.

% 初始化频道状态
initial_channel(ChannelName) ->
  #channel_st{
    name = ChannelName,
    clients = []  % 初始没有任何客户端
  }.

% Start a new server process with the given name
% ServerAtom: 服务器注册的名称（atom）
% 返回: 服务器进程的 PID
start(ServerAtom) ->
  % - Spawn a new process which waits for a message, handles it, then loops infinitely
  % - Register this process to ServerAtom
  % - Return the process ID
  % 使用 genserver 启动服务器进程
  % handleS 是处理服务器消息的回调函数
  genserver:start(ServerAtom, initial_server(ServerAtom), fun handleS/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  % 先请求服务器停止所有频道
  genserver:request(ServerAtom, {stop}),
  % 然后停止服务器自身
  genserver:stop(ServerAtom).

% ============================================================================
% 服务器消息处理器 (handleS)
% 处理发送给主服务器进程的消息
% ============================================================================

% 处理停止服务器的请求
handleS(St, {stop}) ->
  % 遍历所有频道，逐个停止频道进程
  lists:foreach(fun(Channel) ->
    genserver:stop(list_to_atom(Channel))
                end, St#server_st.channels),
  {reply, ok, St};

% 处理客户端加入频道的请求
% Channel: 频道名称（字符串，如 "#chat"）
% Pid: 客户端进程 PID
handleS(St, {join, Channel, Pid}) ->
  % 1. 检查频道进程是否存在
  case whereis(list_to_atom(Channel)) of
    undefined ->
      % 频道不存在，创建新的频道进程
      genserver:start(list_to_atom(Channel),
        initial_channel(Channel),
        fun handleC/2);
    _ ->
      % 频道已存在，不需要创建
      ok
  end,

  % 2. 将加入请求转发给频道进程
  Result = genserver:request(list_to_atom(Channel), {join, Pid}),

  % 3. 如果加入成功，将频道添加到服务器的频道列表中（避免重复）
  NewChannels = case Result of
                  ok ->
                    % 检查频道是否已在列表中
                    case lists:member(Channel, St#server_st.channels) of
                      false -> [Channel | St#server_st.channels];  % 添加新频道
                      true -> St#server_st.channels  % 已存在，不重复添加
                    end;
                  _ ->
                    % 加入失败，不修改频道列表
                    St#server_st.channels
                end,

  % 4. 返回结果和更新后的状态
  {reply, Result, St#server_st{channels = NewChannels}};

% 处理客户端离开频道的请求
handleS(St, {leave, Channel, Pid}) ->
  % 直接将请求转发给频道进程处理
  Result = genserver:request(list_to_atom(Channel), {leave, Pid}),
  {reply, Result, St};

% 处理客户端发送消息的请求
% Channel: 目标频道
% Nick: 发送者的昵称
% Msg: 消息内容
% Sender: 发送者的 PID
handleS(St, {message_send, Channel, Nick, Msg, Sender}) ->
  % 将消息转发给频道进程处理
  Result = genserver:request(list_to_atom(Channel),
    {message_send, Nick, Msg, Sender}),
  {reply, Result, St}.

% ============================================================================
% 频道消息处理器 (handleC)
% 处理发送给频道进程的消息
% ============================================================================

% 处理客户端加入频道
handleC(St, {join, Pid}) ->
  % 检查客户端是否已经在频道中
  case lists:member(Pid, St#channel_st.clients) of
    false ->
      % 客户端不在频道中，添加到成员列表
      {reply, ok, St#channel_st{clients = [Pid | St#channel_st.clients]}};
    true ->
      % 客户端已在频道中，返回错误
      {reply, user_already_joined, St}
  end;

% 处理客户端离开频道
handleC(St, {leave, Pid}) ->
  % 检查客户端是否在频道中
  case lists:member(Pid, St#channel_st.clients) of
    true ->
      % 客户端在频道中，从成员列表中移除
      {reply, ok, St#channel_st{
        clients = lists:delete(Pid, St#channel_st.clients)}};
    false ->
      % 客户端不在频道中，返回错误
      {reply, user_not_joined, St}
  end;

% 处理消息发送
% Nick: 发送者昵称
% Msg: 消息内容
% Sender: 发送者 PID
handleC(St, {message_send, Nick, Msg, Sender}) ->
  % 检查发送者是否在频道中
  case lists:member(Sender, St#channel_st.clients) of
    true ->
      % 发送者在频道中，向所有成员广播消息（包括发送者自己）
      lists:foreach(fun(ClientPid) ->
        % 直接发送消息给客户端进程（异步）
        ClientPid ! {message_receive, St#channel_st.name, Nick, Msg}
                    end, St#channel_st.clients),
      {reply, ok, St};
    false ->
      % 发送者不在频道中，返回错误
      {reply, user_not_joined, St}
  end.