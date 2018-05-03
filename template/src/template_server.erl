%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(template_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([divi/3,
	 add/3]).

-export([test/0]).

-export([start/0,stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {regName}).

%% ====================================================================
%% External support functions
%% ====================================================================

test()->
    Reply=test_myadd:test(),
    Reply.

%% ====================================================================
%% External Server functions
%% ====================================================================
start()-> 
    gen_server:start_link(?MODULE,[],[]).
    
stop(Pid)-> 
    gen_server:call(Pid, {stop}).
 
divi(RegName,A,B)-> 
    gen_server:call(RegName,{divi,A,B},infinity).
add(RegName,A,B)-> 
    gen_server:call(RegName,{add,A,B},infinity).

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
    T=erlang:system_time(nanosecond),
    RegName=list_to_atom(integer_to_list(T)),
    true=erlang:register(RegName,self()),
%    io:format("Application Starting ~p~n",[?MODULE]),
    {ok, #state{regName=RegName}}.

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
handle_call({divi,A,B},_From,State) ->
    Reply=A/B,
    {reply, Reply, State};

handle_call({add,A,B},From,State) ->
    Reply=A+B,
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
  
handle_info({From,{F,Args}}, State) ->
    RegName=State#state.regName,
    Reply=rpc:call(node(),template_lib,F,Args,5000),
    From!{self(),Reply},
    {noreply, State};

handle_info(Info, State) ->
    io:format("~p~n",[{?MODULE,?LINE,Info}]),
    glurk=Info,
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
%% Function: get_all/0
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
