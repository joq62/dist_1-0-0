%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% 
%%% -------------------------------------------------------------------
-module(test_ppool).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
 
%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).
%%
%% API Functions
%%


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
basic_worker_start_stop_test_xx()->
    {ok,Pid1}=template_server:start(),
    {ok,Pid2}=template_server:start(),
    shutdown_ok=template_server:stop(Pid1),
    shutdown_ok=template_server:stop(Pid2),
    ok.

basic_worker_test_xx()->
    {ok,Pid1}=template_server:start(),
    42=template:add(Pid1,20,22),
    5.0=template:divi(Pid1,20,4),
    shutdown_ok=template_server:stop(Pid1),
    ok.

application_ppool_test()->
    ok=application:load(ppool),
    ok=application:start(ppool),
    ok.

start_ppool_test()->
     ppool:start_pool(worker_pool,2,{template,start,[]}),
    {ok,Pid1}=ppool:run(worker_pool,[]),
    {ok,Pid2}=ppool:run(worker_pool,[]),
    noalloc=ppool:run(worker_pool,[]),
    42=template:add(Pid1,20,22),
    5.0=template:divi(Pid1,20,4),
    ok=template:stop(Pid1),
    {ok,Pid3}=ppool:run(worker_pool,[]),
    52=template:add(Pid3,30,22),
    ok=ppool:stop_pool(worker_pool),
    ok.

start_ppool_2_test()->
    {ok,_}=rpc:call(node(),ppool,start_pool,[worker_pool,2,{template,start,[]}],5000),
    {ok,Pid1}=rpc:call(node(),ppool,run,[worker_pool,[]],5000),
    {ok,Pid2}=rpc:call(node(),ppool,run,[worker_pool,[]],5000),
    noalloc=rpc:call(node(),ppool,run,[worker_pool,[]],5000),
    42=rpc:call(node(),template,add,[Pid1,20,22],5000),
    5.0=rpc:call(node(),template,divi,[Pid1,20,4],5000),
    ok=rpc:call(node(),template,stop,[Pid1],5000),
    {ok,Pid3}=rpc:call(node(),ppool,run,[worker_pool,[]],5000),
    52=rpc:call(node(),template,add,[Pid3,30,22],5000),
    ok=rpc:call(node(),ppool,stop_pool,[worker_pool],5000),
    ok.
    

basic_crash_test()->
    {ok,_}=rpc:call(node(),ppool,start_pool,[worker_pool,2,{template,start,[]}],5000),
    {ok,Pid1}=rpc:call(node(),ppool,run,[worker_pool,[]],5000),
    5.0=rpc:call(node(),template,divi,[Pid1,20,4],5000),
    {error,
     [{badrpc,
     {'EXIT',
      {badarith,_}}}]}=rpc:call(node(),template,divi,[Pid1,20,0],5000),
    5.0=rpc:call(node(),template,divi,[Pid1,20,4],5000),
    ok=rpc:call(node(),ppool,stop_pool,[worker_pool],5000),
    ok.


add_crash_test_xx()->
    R1=template:start(node()),
    42=template:add(R1,20,22),
    % wrong parameters
    {error,
     [{badrpc,
     {'EXIT',
      {badarith,_}}}]}=template:add(R1,20,glurk),
    {error,
     [{badrpc,
     {'EXIT',
      {badarith,_}}}]}=template:add(R1,'&',22),
    {R1,stop_ack}=template:stop(R1),
    ok.

divi_crash_test_xxx()->
    R1=template:start(node()),
    5.0=template:divi(R1,20,4),
    % wrong parameters
    {error,
     [{badrpc,
     {'EXIT',
      {badarith,_}}}]}=template:divi(R1,20,glurk),
    {error,
     [{badrpc,
     {'EXIT',
      {badarith,_}}}]}=template:divi(R1,'&',4),

    {R1,stop_ack}=template:stop(R1),
    
    ok.

stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(100),
    erlang:halt().

%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
