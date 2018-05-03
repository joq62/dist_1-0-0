%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_infra_master_stop).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").



%% --------------------------------------------------------------------
-export([]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------


% clean up before stopping
kill_worker_nodes_test()->
    rpc:call('w0@joq-desktop',erlang,halt,[],1000),
    rpc:call('w1@joq-desktop',erlang,halt,[],1000),
    check_if_node_stopped('w1@joq-desktop',10), 
    rpc:call('w2@joq-desktop',erlang,halt,[],1000),
    rpc:call('w3@joq-desktop',erlang,halt,[],1000),
    rpc:call('ppool-1_0_0@joq-desktop',erlang,halt,[],1000),
    ok.

% stop repo_mgr and remove dets file
stop_repo_mgr_test()->
    ok=application:stop(repo_mgr),
    ok=application:unload(repo_mgr),
    file:delete("repo_mgr"),
    ok.

stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.

%%-----------------------------------------------------

check_if_node_stopped(Node,Period)->
    N=do_pang(Node,Period,0,false),
    N.

do_pang(_Node,_Period,N,true)->
    N;
do_pang(Node,Period,N,Quit)->
    case net_adm:ping(Node) of
	pang->
	    NewQuit=true,
	    NewN=N;
	pong->
	    timer:sleep(Period),
	    NewQuit=Quit,
	    NewN=N+1
    end,
    do_pang(Node,Period,NewN,NewQuit).
