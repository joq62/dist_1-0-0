%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_infra_master_load_app).

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
%%  Load ppool and load service tar file - start erlang node with servicename_vsn@localhost
%%  start ppool 
%% --------------------------------------------------------------------

load_ppool_app_test()->
    % Get app info
    AppBaseName="template.app_spec",
    VsnAppFile="1.23.42" ,
    [{"s7","1.7.0",[],[],7,[]},
     {"s3","1.3.0",[],[],3,[]},
     {"s6","1.6.0",[zone_1],[],6,[zone_6]},
     {"s5","1.5.0",[],[],5,[]},
     {"s4","1.4.0",[],[],4,[zone_11,zone_12]},
     {"s2","1.2.0",[zone_1],[],2,[zone_1,zone_2]},
     {"s1","1.1.0",[],[],100,[]}]=josca:start_order(AppBaseName,VsnAppFile),
    {AppBaseName,Binary}=infra_master:get_appfile(AppBaseName,VsnAppFile),
    ok=file:write_file(AppBaseName,Binary),
    {ok,AppInfo}=file:consult(AppBaseName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {needed_services,NeededServiceAppSpecs}=lists:keyfind(needed_services,1,AppInfo),
    {service,ServiceStr}=lists:keyfind(service,1,MainServiceInfo),
    {vsn,VsnStr }=lists:keyfind(vsn,1,MainServiceInfo),
    {board_type,all}=lists:keyfind(board_type,1,MainServiceInfo),
    {numInstances,1}=lists:keyfind(numInstances,1,MainServiceInfo),
    {geo,no}=lists:keyfind(geo,1,MainServiceInfo),
    []=NeededServiceAppSpecs,

%%  Store and unpack tar file att node
    {ok,TarBaseName,BinaryTar}=repo_mgr:get_service(ServiceStr,VsnStr),   
    ok=rpc:call('w0@joq-desktop', file,write_file,[TarBaseName,BinaryTar],5000),
    ok=rpc:call('w0@joq-desktop',erl_tar,extract,[TarBaseName],5000),
    ok=rpc:call('w0@joq-desktop',file,delete,[TarBaseName],5000),

    
   % Start specific erlang node the service 
   % Create NodeStr
   % Create erl command
   % start node
   % start ppool
                    % Create NodeStr
    "joq-desktop"=rpc:call('w0@joq-desktop',net_adm,localhost,[],5000),
    LocalHostStr=rpc:call('w0@joq-desktop',net_adm,localhost,[],5000),
    SpecialVsn=string:join( string:tokens(VsnStr,"."),"_"),
    NodeStr=ServiceStr++"-"++SpecialVsn++"@"++LocalHostStr,

                           % Create erl command & start node
    Path=filename:join(ServiceStr++"-"++VsnStr,"ebin"),
 %   glurk=rpc:call('w0@joq-desktop',file,list_dir,[Path],5000),
    ErlCmd="erl -pa "++Path++" -sname "++NodeStr++" "++"-detached",
    []=rpc:call('w0@joq-desktop',os,cmd,[ErlCmd],5000),
    timer:sleep(500),
    Date=erlang:date(),
    Date=rpc:call('ppool-1_0_0@joq-desktop',erlang,date,[],1000),
    Node=list_to_atom(NodeStr),
    Date=rpc:call(Node,erlang,date,[],1000),

   % start ppool
    Service=list_to_atom(ServiceStr),
    ok=rpc:call(Node,application,load,[Service],1000),
    ok=rpc:call(Node,application,start,[Service],1000),
    {badrpc,nodedown}=rpc:call(Node,erlang,halt,[],1000),
  %
    ok.


%% --------------------------------------------------------------------
%%  Load template and ppool and load service tar file - start erlang node with servicename_vsn@localhost
%%  

load_ppool_app_2_test()->
    % Get app info
    AppBaseName="ppool.app_spec",
    VsnAppFile="1.0.4",
    {AppBaseName,Binary}=infra_master:get_appfile(AppBaseName,VsnAppFile),
    ok=file:write_file(AppBaseName,Binary),
    {ok,AppInfo}=file:consult(AppBaseName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {needed_services,NeededServiceAppSpecs}=lists:keyfind(needed_services,1,AppInfo),
    {service,ServiceStr}=lists:keyfind(service,1,MainServiceInfo),
    {vsn,VsnStr }=lists:keyfind(vsn,1,MainServiceInfo),
    {board_type,all}=lists:keyfind(board_type,1,MainServiceInfo),
    {numInstances,1}=lists:keyfind(numInstances,1,MainServiceInfo),
    {geo,no}=lists:keyfind(geo,1,MainServiceInfo),
    []=NeededServiceAppSpecs,

%%  Store and unpack tar file att node
    {ok,TarBaseName,BinaryTar}=repo_mgr:get_service(ServiceStr,VsnStr),   
    ok=rpc:call('w0@joq-desktop', file,write_file,[TarBaseName,BinaryTar],5000),
    ok=rpc:call('w0@joq-desktop',erl_tar,extract,[TarBaseName],5000),
    ok=rpc:call('w0@joq-desktop',file,delete,[TarBaseName],5000),

    
   % Start specific erlang node the service 
   % Create NodeStr
   % Create erl command
   % start node
   % start ppool
                    % Create NodeStr
    "joq-desktop"=rpc:call('w0@joq-desktop',net_adm,localhost,[],5000),
    LocalHostStr=rpc:call('w0@joq-desktop',net_adm,localhost,[],5000),
    SpecialVsn=string:join( string:tokens(VsnStr,"."),"_"),
    NodeStr=ServiceStr++"-"++SpecialVsn++"@"++LocalHostStr,

                           % Create erl command & start node
    Path=filename:join(ServiceStr++"-"++VsnStr,"ebin"),
 %   glurk=rpc:call('w0@joq-desktop',file,list_dir,[Path],5000),
    ErlCmd="erl -pa "++Path++" -sname "++NodeStr++" "++"-detached",
    []=rpc:call('w0@joq-desktop',os,cmd,[ErlCmd],5000),
    timer:sleep(500),
    Date=erlang:date(),
    Date=rpc:call('ppool-1_0_0@joq-desktop',erlang,date,[],1000),
    Node=list_to_atom(NodeStr),
    Date=rpc:call(Node,erlang,date,[],1000),

   % start ppool
    Service=list_to_atom(ServiceStr),
    ok=rpc:call(Node,application,load,[Service],1000),
    ok=rpc:call(Node,application,start,[Service],1000),
    {badrpc,nodedown}=rpc:call(Node,erlang,halt,[],1000),
  %
    ok.


%%%%  Check application management


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
	    
