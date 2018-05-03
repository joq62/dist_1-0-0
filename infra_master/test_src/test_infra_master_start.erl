%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_infra_master_start).

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

%% --------------------------------------------------------------------
%% 1. Initial set up
%% --------------------------------------------------------------------
delete_dets_file_test()->
    file:delete("infra_master"),
    file:delete("repo_mgr"),
    ok.

secure_kill_worker_nodes_test()->
    rpc:call('w0@joq-desktop',erlang,halt,[],1000),
    timer:sleep(100),
    rpc:call('w1@joq-desktop',erlang,halt,[],1000),
    timer:sleep(100),
    rpc:call('w2@joq-desktop',erlang,halt,[],1000),
    timer:sleep(100),
    rpc:call('w3@joq-desktop',erlang,halt,[],1000),
    timer:sleep(100),
    rpc:call('service_100@joq-desktop',erlang,halt,[],1000),
    timer:sleep(100),
    rpc:call('ppool-1_0_0@joq-desktop',erlang,halt,[],1000),
    ok.

%% --------------------------------------------------------------------
%% 3. Start repo_mgr and sinfra_mastertore template and ppool 
%% --------------------------------------------------------------------

start_repo_mgr_test()->
    ok=application:load(repo_mgr),
    ok=application:start(repo_mgr),
    ok.



%% --------------------------------------------------------------------
%% 2. Check handling workers  
%% --------------------------------------------------------------------
start_workers_test()->
    c:cd("../test_area/w0"),
    os:cmd("erl -pa brd_mgr/ebin -run brd_mgr start_brd_mgr -sname w0 -detached"),
    check_if_node_running('w0@joq-desktop',10),
    c:cd("../w1"),
    os:cmd("erl -pa brd_mgr/ebin -run brd_mgr start_brd_mgr -sname w1 -detached"),
    check_if_node_running('w1@joq-desktop',10),
    c:cd("../w2"),
    os:cmd("erl -pa brd_mgr/ebin -run brd_mgr start_brd_mgr -sname w2 -detached"),
    check_if_node_running('w2@joq-desktop',10),
  %  c:cd("../w3"),
  %  os:cmd("erl -pa brd_mgr/ebin -run brd_mgr start_brd_mgr -sname w3 -detached"),
   % timer:sleep(100),
    c:cd("../../infra_master"),
    ok.

start_test()->
    {ok,ok}=infra_master:start_infra(),
    ok.
check_allWorkers_test()->
    ["w0@joq-desktop","w1@joq-desktop","w2@joq-desktop"]=infra_master:get_active(),
    ["w3@joq-desktop"]=infra_master:get_disconnected(),
    ok.


%% testing starting a node on a board

test_start_service_node_test()->
    []=rpc:call('w0@joq-desktop',os,cmd,["erl -pa brd_mgr/ebin -sname service_100 -detached"],5000),
    %rpc:call("w0@joq-desktop",os,cmd,["erl -sname z -detached"],5000),
   % glurk=rpc:call("w0@joq-desktop",brd_mgr,rpc_call,[{erlang,time,[]}],5000),
    check_if_node_running('service_100@joq-desktop',10),
    ['w0@joq-desktop',
     'w1@joq-desktop',
     'w2@joq-desktop',
     'service_100@joq-desktop']=nodes(),
    ok.

kill_service_node_test()->
    rpc:call('service_100@joq-desktop',erlang,halt,[],1000),
    timer:sleep(100),
    ok.


%-- System running - Ready for further testing

check_if_node_running(Node,Period)->
    N=do_ping(Node,Period,0,false),
    N.

do_ping(_Node,_Period,N,true)->
    N;
do_ping(Node,Period,N,Quit)->
    case net_adm:ping(Node) of
	pong->
	    NewQuit=true,
	    NewN=N;
	pang->
	    timer:sleep(Period),
	    NewQuit=Quit,
	    NewN=N+1
    end,
    do_ping(Node,Period,NewN,NewQuit).

