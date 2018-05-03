%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(brd_mgr).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
% -include("../infra_master/include/infra.hrl").
%% --------------------------------------------------------------------
-define(CONFIGFILE,"service.config").


%% External exports
-export([rpc_call/1]).


-export([start/0,stop/0]).
-export([start_brd_mgr/0,stop_brd_mgr/0]).


%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External support functions
%% ====================================================================


start_brd_mgr()->
    Load=application:load(?MODULE),
    Start=application:start(?MODULE),
    {Load,Start}.

stop_brd_mgr()->
    Stop=application:stop(?MODULE),
    UnLoad=application:unload(?MODULE),
    {Stop,UnLoad}.


%% ====================================================================
%% External Server functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

rpc_call({M,F,Args})->
    gen_server:call(?MODULE, {rpc_call,{M,F,Args}},infinity).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok,ConfigInfo}=file:consult(?CONFIGFILE),
    {infra_master_node,NodeInfraMaster}=lists:keyfind(infra_master_node,1,ConfigInfo),
    true=connect_master_node(NodeInfraMaster,10000),
    io:format("Application Starting ~p~n",[{?MODULE}]),
    {ok, #state{}}.
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
handle_call({rpc_call,{M,F,Args}}, _From, State) ->
    Reply = rpc:call(node(),M,F,Args),
    {reply, Reply, State};
% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
connect_master_node(NodeInfraMaster,Period)->
    R=connect_master_node(NodeInfraMaster,Period,false),
    R.

connect_master_node(NodeInfraMaster,Period,false)->
    case net_adm:ping(NodeInfraMaster) of
	pong->
	    NewR=true;
	_ ->
	    NewR=false,
	    timer:sleep(Period)
    end,
    connect_master_node(NodeInfraMaster,Period,NewR);

connect_master_node(_NodeInfraMaster,_Period,R)->
    R.
