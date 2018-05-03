%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(template).

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

%% ====================================================================
%% External support functions
%% ====================================================================

test()->
    Reply=test_template_server:test(),
    Reply.

%% ====================================================================
%% Api external Server functions
%% ====================================================================

start()-> 
    Reply=rpc:call(node(),template_server,start,[]),
    Reply.


stop(Resource)-> 
    Reply=rpc:call(node(),template_server,stop,[Resource],5000),
    ok.


%% ====================================================================
%% Api application service functions
%% ====================================================================


add(Resource,A,B)->
    Resource!{self(),{add,[A,B]}},
    receive
	{Resource,{add_ack,Result}}->
	    Reply=Result;
	{Resource,Err}->
	    Reply={error,[Err]};
	Err->
	    Reply={error,[Err]}
    after 5000 ->
	    Reply={error,[?MODULE,?LINE,'time_out']}
    end,
    Reply.

divi(Resource,A,B)-> 
    Resource!{self(),{divi,[A,B]}},
    receive
	{Resource,{divi_ack,Result}}->
	    Reply=Result;
	{Resource,Err}->
	    Reply={error,[Err]};
	    Err->
	    Reply={error,[Err]}
    after 5000 ->
	    Reply={error,[?MODULE,?LINE,'time_out']}
    end,
    Reply.
