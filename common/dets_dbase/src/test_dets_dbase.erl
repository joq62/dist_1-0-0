%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% 
%%% -------------------------------------------------------------------
-module(test_dets_dbase).
 
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

start_test()->
    ok.

create_set_test()->
    {ok,file_created}=dets_dbase:create(?MODULE,[{type,set}]),
    ok.

create_object_test()->
    {ok,store}=dets_dbase:create_object({addr,"localhost",10501},[{addr,"localhost",10501},
					   {location,zone_0},
					   {capability,[]},{appl,[]}],?MODULE),
    {ok,store}=dets_dbase:create_object({addr,"localhost",10511},[{addr,"localhost",10511},
					   {location,zone_1},
					   {capability,[dbase,tellstick]},{appl,[]}],?MODULE),
    {ok,store}=dets_dbase:create_object({addr,"localhost",10521},[{addr,"localhost",10521},
					   {location,zone_2},
					   {capability,[tellstick]},{appl,[]}],?MODULE),

    ok.
all_test()->
 %   {ok,R}=dets:open_file(?MODULE),
  %  glurk=dets:match(R,{{addr,'$1'},'$3'}),
    L=dets_dbase:match({{addr,'_','_'},'$3'},?MODULE),
    L1=[Z||[Z]<-L],
    [{"localhost",10511,
      [{location,zone_1},{capability,[dbase,tellstick]},{appl,[]}]}]=[{Ip,Port,T}||[{addr,Ip,Port}|T]<-L1,Ip=="localhost",Port==10511],
    dets:close(?MODULE),
%    A=[[[{addr,"localhost",10501},{location,zone_0},{capability,[]},{appl,[]}]],
 %      [[{addr,"localhost",10511},{location,zone_1},{capability,[dbase,tellstick]},{appl,[]}]],
  %     [[{addr,"localhost",10521},{location,zone_2},{capability,[tellstick]},{appl,[]}]]],

    ok.


stop_test()->   
    file:delete(?MODULE),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.

%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
