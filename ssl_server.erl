%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2013, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created : 12 Jan 2013 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(ssl_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(SSLHANDLERS, {pubsub, sslbroadcast}).
-define(MAX_STANDBY, 15 * 60 * 1000).

-record(state, {buffer = <<>>, socket, tref}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(LSocket) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSocket) ->
    gen_server:start_link(?MODULE, [LSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([LSocket]) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket = LSocket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(accept, #state{socket = LSocket} = State) ->
    %% Set again socket in active mode, SSL bug?
    %% Check it using: io:format("~p~n", [ssl:getopts(LSocket, [active])]),
    ssl:setopts(LSocket, [{active, once}]),
    {ok, NewSocket} = ssl:transport_accept(LSocket),
    ok = ssl:ssl_accept(NewSocket),
    ssl_server_sup:start_socket(),
     
    gproc:reg({p, l, ?SSLHANDLERS}),
    {ok, TRef} = erlang:send_after(?MAX_STANDBY, self(), disconnect),
    {noreply, State#state{socket = NewSocket, tref = TRef}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Pid, ?SSLHANDLERS, _Msg}, State) when Pid =:= self() ->
    {noreply, State};

handle_info({_Pid, ?SSLHANDLERS, Msg}, State) ->
    io:format("Received message: ~p~n", [Msg]),
    {noreply, State};

handle_info({ssl, Socket, Data}, #state{buffer = Buffer, tref = TRef} = State) ->
    erlang:cancel_timer(TRef),
    ssl:setopts(Socket, [{active, once}]),
    NewBuffer = parse_buffer(<<Buffer/binary, Data/binary>>),
    {ok, TRef} = erlang:send_after(?MAX_STANDBY, self(), disconnect),
    {noreply, State#state{buffer = NewBuffer, tref = TRef}};

handle_info({ssl_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(disconnect, #state{socket = Socket} = State) ->
    ssl:close(Socket), 
    {stop, normal, State};

handle_info(_Info, State) ->
    io:format("Received unknown message: ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_buffer(Buffer) ->
    case Buffer of
	<<1:8, PayloadSize:16/big, Payload:PayloadSize/binary-unit:8, Rest/binary>> ->
	    forward_message(Payload),
	    parse_buffer(Rest);
	_ -> 
	    Buffer
    end.
    
forward_message(Message) ->
    gproc:send({p, l, ?SSLHANDLERS}, {self(), ?SSLHANDLERS, Message}).
