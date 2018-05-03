%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(infra_master_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------

%% External exports
-export([get_appfile/3,store_appfile/4,
	 load_service/5,unload_service/3,service_loaded/3,
	 store_appfile/3
	 ]).

-export([service_deployed/3,delete_deployed/4,
	 start_service/4,stop_service/4,test_service/3,
	 candidate_workers/2,
	 make_service_available/4
	]).

-export([add_all_workers/1,
	 add_all_workers/3,
	 connect_workers/1,
	 poll_workers/2,
	 poll_workers/3
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions

%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
store_appfile(AppBaseName,VsnAppFile,Binary,DbaseFile)->
    case dets_dbase:member_object({appfile,AppBaseName,VsnAppFile},DbaseFile) of
	false->
	    Key={appfile,AppBaseName,VsnAppFile},
	    Value={AppBaseName,Binary},
	    Reply=dets_dbase:create_object(Key,Value,DbaseFile);
	true ->
	    Key={appfile,AppBaseName,VsnAppFile},
	    NewValue={object,{AppBaseName,Binary}},
	    Reply=dets_dbase:update_object(NewValue,Key,DbaseFile)
    end,
    Reply.
	    


%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
load_service(Node,ServiceStr,VsnStr,TarBaseName,TarBinary)->
    ok=rpc:call(Node,file,write_file,[TarBaseName,TarBinary],5000),
    ok=rpc:call(Node,erl_tar,extract,[TarBaseName],5000),
    ok=rpc:call(Node,delete,[TarBaseName],5000),
    Reply={'Loaded service ',ServiceStr,' with version ',VsnStr,' on worker ',Node},
    Reply.

%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
unload_service(Node,ServiceStr,VsnStr)->
    RmCmd="rm -r "++ServiceStr++"_"++VsnStr,
    Reply=rpc:call(Node,os,cmd,file,[RmCmd],5000),
    Reply.
%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
service_loaded(_Node,_Service,_Vsn)->
    ok.



%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
get_appfile(AppFileName,Vsn,DbaseFile) ->
    case dets_dbase:member_object({appfile,AppFileName,Vsn},DbaseFile) of
	true->
	    Key={appfile,AppFileName,Vsn},
	    Reply=dets_dbase:read_object(object,Key,DbaseFile);
	false->
	    Reply={error,[?MODULE,?LINE,AppFileName,Vsn,' is not stored in dbase']}
    end,	    
    Reply.



%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
store_appfile(AppFileName,Binary,DbaseFile) ->
    ok=file:write_file(AppFileName,Binary),
    {ok,AppInfo}=file:consult(AppFileName),
    ok=file:delete(AppFileName),
    {vsn,VsnAppFile}=lists:keyfind(vsn,1,AppInfo),
    case dets_dbase:member_object({appfile,AppFileName,VsnAppFile},DbaseFile) of
	true->
	    Reply={error,[?MODULE,?LINE,'allredy exists ',AppFileName,VsnAppFile]};
	false->
	    Key={appfile,AppFileName,VsnAppFile},
	    Value=Binary,
	    case dets_dbase:create_object(Key,Value,DbaseFile) of
		{error,Err}->
		    Reply={error,[?MODULE,?LINE,Err]};
		{ok,store}->
		    Reply=ok
	    end
    end,
    Reply.	    


%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------


make_service_available(ServiceStr,IpAddr,Port,ActiveWorkers) ->
    sd:add_resource(ServiceStr,IpAddr,Port), % Add to infra master
    Result=[{{IpWorker,PortWorker},sd:call(IpWorker,PortWorker,sd,add_resource,[ServiceStr,IpAddr,Port],5000)}||{IpWorker,PortWorker}<-ActiveWorkers],
    Result.
%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
%[{addr,"localhost",10501},{location,zone_0},{capability,[]},{appl,[]}]


candidate_workers(NeededCapabilities,ActiveWorkers)->
    ActiveWorkersCapablities=[{IpAddr,Port,Capabilities}||[{addr,IpAddr,Port},{location,_Location},{capability,Capabilities},{appl,_Apps}]<-ActiveWorkers],
    UnFilteredCapabilities=[{IpAddr,Port,lists:member(X,WorkerCapability)}||X<-NeededCapabilities,{IpAddr,Port,WorkerCapability}<-ActiveWorkersCapablities],
    FilteredWorkers=[{IpAddr,Port}||{IpAddr,Port,true}<-UnFilteredCapabilities],
    FilteredWorkers.



%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
test_service(ServiceStr,IpAddr,Port)->
    ServiceAtom=list_to_atom(ServiceStr),
    case sd:call(IpAddr,Port,ServiceAtom,test,[],5000) of
	ok->
	    Reply={ok,ServiceStr,IpAddr,Port};
	Err ->
	    Reply={failed,ServiceStr,IpAddr,Port,Err}
		
    end,
    Reply.

%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------

% application:which_applications(). 
% [{brd_mgr,"brd_mgr server using dets ","1.0.0"},
% {stdlib,"ERTS  CXC 138 10","3.3"},
% {kernel,"ERTS  CXC 138 10","5.2"}]

start_service(ServiceStr,Vsn,IpAddr,Port)->
    case sd:call(IpAddr,Port,application,which_applications,[],5000) of
	{error,_Err}->
	    Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',Vsn,' is not running on worker ',IpAddr,Port]}};
	RunningServices->
	    ServiceAtom=list_to_atom(ServiceStr),
	    case lists:keyfind(ServiceAtom,1,RunningServices) of
		false->
		    Reply=load_start_service(ServiceStr,Vsn,IpAddr,Port);
		{ServiceAtom,_Info,Vsn}-> %Service is running on Worker 
		     Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',Vsn,' is already running on worker ',IpAddr,Port]}};
		{ServiceAtom,_Info,OtherVsn}->  %Service with another version is already running 
		     Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',OtherVsn,' is running on worker (need to be stoped first ',IpAddr,Port]}}
	    end
    end,
    Reply.

%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------

% application:which_applications(). 
% [{brd_mgr,"brd_mgr server using dets ","1.0.0"},
% {stdlib,"ERTS  CXC 138 10","3.3"},
% {kernel,"ERTS  CXC 138 10","5.2"}]

stop_service(ServiceStr,Vsn,IpAddr,Port)->
    case sd:call(IpAddr,Port,application,which_applications,[],5000) of
	{error,_Err}->
	    Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',Vsn,' is not running on worker ',IpAddr,Port]}};
	RunningServices->
	    ServiceAtom=list_to_atom(ServiceStr),
	    case lists:keyfind(ServiceAtom,1,RunningServices) of
		false->
		    Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',Vsn,' is not running on worker ',IpAddr,Port]}};
		{ServiceAtom,_Info,Vsn}-> %Service is running on Worker 
		    Reply=stop_unload_service(ServiceStr,Vsn,IpAddr,Port);
		{ServiceAtom,_Info,_OtherVsn}->  %Service with Vsn is not running on Worker 
		     Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',Vsn,' is not running on worker ',IpAddr,Port]}}
	    end
    end,
    Reply.


%% --------------------------------------------------------------------
%% @spec:  delete_deployed(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
delete_deployed(ServiceStr,Vsn,IpAddr,Port)->	    
    case service_deployed(ServiceStr,IpAddr,Port) of
	false->
	    Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',Vsn,' is not deployed on worker',IpAddr,Port]}};
	{true,Deployed}->  %[{Dir,ServiceStr,Vsn}]
	    case lists:keyfind(Vsn,3,Deployed) of
		false-> % Earlier versions of services are deployed
		    Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',Vsn,' is not deployed on worker',IpAddr,Port]}};
		{Dir,ServiceStr,Vsn}-> % service is deployed 
		    Reply=sd:call(IpAddr,Port,os,cmd,["rm -r "++Dir],5000)
	    end
    end,
    Reply.
%% --------------------------------------------------------------------
%% @spec:  load_service(ServiceStr,Vsn,Worker) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
load_service(ServiceStr,Vsn,IpAddr,Port,DbaseFile,glurk)->	    
    case dets_dbase:member_object({service,ServiceStr,Vsn},DbaseFile) of 
	false->
	    Reply={error,{?MODULE,?LINE,['service ',ServiceStr,' with vsn ',Vsn,' does not exists in repository']}};
	true ->
	    case service_deployed(ServiceStr,IpAddr,Port) of
		false->
		    Reply=load_sw(ServiceStr,Vsn,IpAddr,Port);
		{true,Deployed}->  %[{Dir,ServiceStr,Vsn}]
		    case lists:keyfind(Vsn,3,Deployed) of
			false-> % Earlier versions of services are deployed
			    Reply=load_sw(ServiceStr,Vsn,IpAddr,Port);
			{Dir,ServiceStr,Vsn}-> % service is deployed 
			    Reply={error,{?MODULE,?LINE,[ServiceStr,already_deployed]}}
		    end
	    end
    end,
    Reply.

%% --------------------------------------------------------------------
%% @spec:  load_service(ServiceStr,IpAddr,Port) 
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
load_service(ServiceStr,IpAddr,Port)->	    
    case dets_dbase:read_object(vsn,{latest,ServiceStr}) of 
	{error,Err}->
%	    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
	    Reply={error,[IpAddr,Port,ServiceStr,Err]};
	VsnLatest ->
	    case service_exists(ServiceStr,IpAddr,Port) of
		false->
		    io:format("not exists ~p~n",[{?MODULE,?LINE,ServiceStr,IpAddr,Port}]),
		    Reply=load_sw(ServiceStr,VsnLatest,IpAddr,Port);
		AppFiles ->
	%	    io:format(" exists ~p~n",[{?MODULE,?LINE,ServiceStr,IpAddr,Port,AppFiles}]),
		    Reply=load_sw(ServiceStr,IpAddr,Port,VsnLatest,AppFiles)
	    end
    end,
    Reply.



%% --------------------------------------------------------------------
%% @spec:  service_deployed(ServiceStr,IpAddr,Port)
%% @spec:  false | {true,[Vsn1,Vsn2.VsnN]}
%% Returns: non
%% --------------------------------------------------------------------
service_deployed(ServiceStr,IpAddr,Port)->
    case sd:call(IpAddr,Port,file,list_dir,["."],5000) of
	{error,Err}->
	    Reply={error,[IpAddr,Port,ServiceStr,Err]};
     {ok,Files}->
	    ServiceAtom=list_to_atom(ServiceStr),
	    AppFilesInfo=[{Dir, sd:call(IpAddr,Port,file,consult,[filename:join([Dir,"ebin",ServiceStr++".app"])],5000)} || Dir<-Files],
	    Deployed=[{Dir,ServiceStr,Vsn}||{Dir,{ok,[{application,ServiceAtom,[_,{vsn,Vsn},_,_,_,_,_]}]}}<-AppFilesInfo],
	    case Deployed of
		[]->
		    Reply=false;
		_->
		    Reply={true,Deployed}
	   end
    end,
    Reply.


 %% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
add_all_workers(DbaseFile)->
    {ok,R}=dets:open_file(DbaseFile),
    WorkersDetsList=dets:match(R,{{worker,'$1'},'$2'}),
    WorkersList=fix_worker_list(WorkersDetsList,[]),
    Reply=rpc:call(node(),?MODULE,add_all_workers,[WorkersList,DbaseFile,ok],300),
    Reply.

add_all_workers([],_DbaseFile,Acc)->
    Acc;
add_all_workers([Worker|T],DbaseFile,_Acc)->
     case dets_dbase:read_object(object,{worker,Worker},DbaseFile) of
	 [{brd_ipaddr,IpAddr},
	  {brd_port,BrdMgrPort},
	  {location,_},
	  {capability,_},
	  {appl,_}]->
	     true=sd:add_resource(Worker,IpAddr,BrdMgrPort),
	    % true=sd:add_resource(Wrk,IpAddr,WrkPort),
	     NewAcc=ok;
	 Err ->
	     NewAcc={error,{?MODULE,?LINE,Err}}
     end,
    add_all_workers(T,DbaseFile,NewAcc).


fix_worker_list([],Acc)->
    Acc;
fix_worker_list([[IdWorker|_]|T],Acc) ->
    NewAcc=[IdWorker|Acc],
    fix_worker_list(T,NewAcc).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
poll_workers(start,Interval,DbaseFile)->
 %   io:format("~p~n",[{?MODULE,?LINE,start,Interval}]),
    infra_master:connect_workers(Interval,DbaseFile).
poll_workers(Interval,DbaseFile)->
%    io:format("~p~n",[{?MODULE,?LINE,Interval}]),
    timer:sleep(Interval),
    infra_master:connect_workers(Interval,DbaseFile).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

connect_workers(DbaseFile)->
 %   io:format("~p~n",[{?MODULE,?LINE}]),
    L=dets_dbase:match({{addr,'_','_'},'$3'},DbaseFile),
    AllWorkers=[Z||[Z]<-L],
    TestedWorkers=[{connect_worker(IpAddr,Port),WorkerInfo}||[{addr,IpAddr,Port}|WorkerInfo] <-AllWorkers],   
    Active=[lists:append([{addr,IpAddr,Port}],WorkerInfo)||{{active,IpAddr,Port,_},WorkerInfo} <-TestedWorkers],
    Standby=[lists:append([{addr,IpAddr,Port}],WorkerInfo)||{{standby,IpAddr,Port,_},WorkerInfo}<-TestedWorkers],
    Disconnected=[lists:append([{addr,IpAddr,Port}],Err)||{{error,IpAddr,Port,Err},WorkerInfo}<-TestedWorkers],   


%    case dets_dbase:matchid_all_workers(DbaseFile) of
%	{ok,Workers}->
%	    io:format("~p~n",[{?MODULE,?LINE}]),
	    %% Try to connect all workers
%	    ConnectionWorkers=[connect_worker(Worker,DbaseFile) || Worker<-Workers],
%	    Active=[Worker||{active,Worker,_}<-ConnectionWorkers],
%	    Standby=[Worker||{standby,Worker,_}<-ConnectionWorkers],
%	    Disconnected=[{Worker,Err}||{error,Worker,Err}<-ConnectionWorkers],

%	    io:format("~p~n",[{?MODULE,?LINE,Active,Standby}]),
	    % Update local sd with the ones just started
%	    LocalSdActive=[update_local_sd(Worker,DbaseFile) || Worker<-Active],
%	    io:format("LocalSdActive ~p~n",[{?MODULE,?LINE,LocalSdActive}]),
%	    LocalSdStandby=[update_local_sd(Worker,DbaseFile) || Worker<-Standby],
%	    io:format("Standby ~p~n",[{?MODULE,?LINE,LocalSdStandby}]),
	   
            %load and test services
%	    LoadedServices=[load_services(Worker) || Worker<-Standby],
%	    LoadedServicesList=lists:append(LoadedServices),
%	    [load_start_service(ServiceStrNoUpdate,VsnNoUpdate,WorkerNoUpdate) ||{no_update,ServiceStrNoUpdate,VsnNoUpdate,WorkerNoUpdate}<-LoadedServicesList],
	    
%	    io:format("LoadedServices ~p~n",[{?MODULE,?LINE,LoadedServices}]),
%	    TestedServices=[do_test_service(ServiceStr2Test,Vsn2Test,Worker2Test) ||{updated,ServiceStr2Test,Vsn2Test,Worker2Test}<-LoadedServicesList],

%	    OkServices=[{Worker,ServiceStr,Vsn} ||{ok,Worker,ServiceStr,Vsn}<-Loaded_Tested],
%	    FailedServices=[{Worker,[Err]} ||{error,Worker,[Err]}<-Loaded_Tested],
	    
	    %remove_sd(UpdatedDisconnected),
	    Reply=[{active,Active},
		   {standby,Standby},
		   {disconnected,Disconnected}],
	%	   {loaded_services,LoadedServicesList},
	%	   {tested_services,TestedServices}];
	%	   {ok_services,OkServices},{failed_services,FailedServices}]; 
	  %  Reply={[Active|UpdatedActive],[Disconnected|UpdatedDisconnected]};
%	Err->
%	    Reply={error,{?MODULE,?LINE,Err}}
 %   end,      
    Reply.

%% --------------------------------------------------------------------
%% Function: connect_worker(Worker,DbaseFile) 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

connect_worker(IpAddr,Port) ->
 %   io:format("~p~n",[{?MODULE,?LINE,Worker}]),
    case sd:call(IpAddr,Port,brd_mgr,connect,[],5000) of
	active->
%	    io:format("~p~n",[{?MODULE,?LINE,Worker}]),
	    Reply={active,IpAddr,Port,[]};   
	standby ->
%	    io:format("~p~n",[{?MODULE,?LINE,Worker}]),
	    Reply={standby,IpAddr,Port,[]};  
	{error,Err}->
%	    io:format("~p~n",[{?MODULE,?LINE,Worker,error,Err}]),
	    Reply={error,IpAddr,Port,[Err]};   
	Err ->
%	    io:format("~p~n",[{?MODULE,?LINE,Worker,Err}]),
	    Reply={error,IpAddr,Port,[Err]}   
    end,				
 %   io:format("~p~n",[{?MODULE,?LINE}]),  
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------



stop_unload_service(ServiceStr,Vsn,IpAddr,Port)->
    Ebin=filename:join(ServiceStr++"-"++Vsn,"ebin"),
    ServiceAtom=list_to_atom(ServiceStr),
    Stop=sd:call({IpAddr,Port,brd_mgr},application,stop,[ServiceAtom],5000),
    Unload=sd:call({IpAddr,Port,brd_mgr},application,unload,[ServiceAtom],5000),
    true=sd:call({IpAddr,Port,brd_mgr},code,del_path,[Ebin],5000),
    case sd:call({IpAddr,Port,brd_mgr},file,list_dir,[Ebin],5000) of
	{ok,BaseNames}->
	    Reply=[purge_file(BaseName,IpAddr,Port,Ebin) || BaseName<-BaseNames];
	{error,Err}->
	    Reply={error,[?MODULE,?LINE,ServiceStr,Vsn,IpAddr,Port,Err]}
    end,
    Reply.

load_start_service(ServiceStr,Vsn,IpAddr,Port)->
    Ebin=filename:join(ServiceStr++"-"++Vsn,"ebin"),
    ServiceAtom=list_to_atom(ServiceStr),
    case sd:call({IpAddr,Port,brd_mgr},application,unload,[ServiceAtom],5000) of
	{error,{running,ServiceAtom}}->
	    Reply={error,[?MODULE,?LINE,already_started,ServiceStr,Vsn,IpAddr,Port]};
	{error,{not_loaded,ServiceAtom}}->
	    case sd:call({IpAddr,Port,brd_mgr},code,add_patha,[Ebin],5000) of
		{error,Err}->
		      Reply={error,[?MODULE,?LINE,ServiceStr,Vsn,IpAddr,Port,Err]};
		true->
		    case sd:call({IpAddr,Port,brd_mgr},file,list_dir,[Ebin],5000) of
			{ok,BaseNames}->
			    [load_file(BaseName,IpAddr,Port,Ebin) || BaseName<-BaseNames],
			    Load=sd:call({IpAddr,Port,brd_mgr},application,load,[ServiceAtom],5000),
			    Start=sd:call({IpAddr,Port,brd_mgr},application,start,[ServiceAtom],5000),
			    Reply={Load,Start,ServiceStr,Vsn,IpAddr,Port};
			{error,Err}->
			    Reply={error,[?MODULE,?LINE,ServiceStr,Vsn,IpAddr,Port,Err]}
		    end
	    end
    end,
    Reply.

load_file(BaseName,IpAddr,Port,Ebin) ->
    case filename:extension(BaseName) of
	".beam"->
	    ModuleStr=filename:basename(BaseName,".beam"),
	    Module=list_to_atom(ModuleStr),
%	    io:format("load Module ~p~n",[{?MODULE,?LINE,Module}]),
	    Reply=sd:call(IpAddr,Port,code,load_file,[Module],5000);
	NoBeamFile ->
	    Reply={do_nothing,NoBeamFile}
    end,
    Reply.

purge_file(BaseName,IpAddr,Port,Ebin) ->
    case filename:extension(BaseName) of
	".beam"->
	    ModuleStr=filename:basename(BaseName,".beam"),
	    Module=list_to_atom(ModuleStr),
%	    io:format("Purge  Module ~p~n",[{?MODULE,?LINE,Module}]),
	    Reply={sd:call(IpAddr,Port,code,purge,[Module],5000),BaseName};
	NoBeamFile->
	   % io:format("do nothing ~p~n",[{?MODULE,?LINE,Err}]),
	    Reply={do_nothing,NoBeamFile}
    end,
    Reply.

    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
 do_test_service(ServiceStr,VsnNew,IpAddr,Port)->
    
    LoadedApplications=sd:call({IpAddr,Port,brd_mgr},application,loaded_applications,[],5000),
    ServiceAtom=list_to_atom(ServiceStr),
    case lists:keyfind(ServiceAtom,1,LoadedApplications) of
	false->
	    load_start_service(ServiceStr,VsnNew,IpAddr,Port),
	    case sd:call({IpAddr,Port,brd_mgr},ServiceAtom,test,[],5000) of
		ok->
		    Reply={ok,ServiceStr,IpAddr,Port};
		Err ->
		    stop_unload_service(ServiceStr,VsnNew,IpAddr,Port),
		    ServiceDirNew=ServiceStr++"-"++VsnNew,
		    sd:call({IpAddr,Port,brd_mgr},os,cmd,["rm -r "++ServiceDirNew],5000),
		    Reply={failed,ServiceStr,IpAddr,Port,Err}
	    end;
	{ServiceAtom,_AppInfo,VsnNew}-> %% Already latest
	    Reply={ok,ServiceStr,IpAddr,Port};
	{ServiceAtom,_AppInfo,Vsn}->
	    stop_unload_service(ServiceStr,Vsn,IpAddr,Port),
	    load_start_service(ServiceStr,VsnNew,IpAddr,Port),
	    case sd:call({IpAddr,Port,brd_mgr},ServiceAtom,test,[],5000) of
		ok->
		    ServiceDirCurrent=ServiceStr++"-"++Vsn,
		     io:format("ServiceDirCurrent~p~n",[{?MODULE,?LINE,ServiceDirCurrent}]),
		    sd:call({IpAddr,Port,brd_mgr},os,cmd,["rm -r "++ServiceDirCurrent],5000),
		    Reply={ok,ServiceStr,IpAddr,Port};
		Err ->
		    stop_unload_service(ServiceStr,VsnNew,IpAddr,Port),
		    load_start_service(ServiceStr,Vsn,IpAddr,Port),
		    ServiceDirNew=ServiceStr++"-"++VsnNew,
		    sd:call({IpAddr,Port,brd_mgr},os,cmd,["rm -r "++ServiceDirNew],5000),
		    Reply={failed,ServiceStr,IpAddr,Port,Err}
	    end
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
update_local_sd(Worker,DbaseFile)->
    [{brd_ipaddr,IpAddr},
     {brd_port,BrdPort},
     {location,_},
     {capability,_},
     {appl,_}]=dets_dbase:read_object(object,{worker,Worker},DbaseFile),
 %   Resource=Worker++"-"++"brd_mgr",
    true=sd:add_resource({Worker,brd_mgr},IpAddr,BrdPort),
    {Worker,IpAddr,BrdPort}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_services(IpAddr,Port) ->
%    io:format("~p~n",[{?MODULE,?LINE,Worker}]),
    case dets_dbase:read_object(services,{deployment,IpAddr,Port}) of
	{error,Err}->
	    % Worker has no Services to deploy
%	    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
	    Reply={error,IpAddr,Port,[Err]};
	ServicesToDeploy ->
	    io:format("~p~n",[{?MODULE,?LINE,ServicesToDeploy}]),
	    LoadedServices=[load_service(ServiceStr,IpAddr,Port) || {ServiceStr,_Num}<-ServicesToDeploy],
	    Reply=LoadedServices
	    %Reply=[deploy_service(ServiceStr,Worker) || {ServiceStr,_Num}<-ServicesToDeploy]
    end,
    Reply.
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

load_sw(ServiceStr,IpAddr,Port,VsnLatest,AppFiles)->
    Compared=[compare_vsn(VsnLatest,ServiceStr,DirFile,Binary)||{ServiceStr,DirFile,Binary,IpAddr,Port}<-AppFiles],
    io:format(" compared ~p~n",[{?MODULE,?LINE,ServiceStr,IpAddr,Port,Compared}]),
    case {lists:keymember(less,1,Compared),lists:keymember(equal,1,Compared)} of
	{false,false}->
%	    io:format(" {false,false} ~p~n",[{?MODULE,?LINE}]),
	    Reply=load_sw(ServiceStr,VsnLatest,IpAddr,Port);	    
	_->
	   % io:format(" {A,B} ~p~n",[{?MODULE,?LINE,A,B}]),
	    Reply={no_update,ServiceStr,VsnLatest,IpAddr,Port}
    end,
    Reply.

load_sw(ServiceStr,Vsn,IpAddr,Port)->	
    [TarBaseName,Binary]=dets_dbase:read_object(release,{service,ServiceStr,Vsn}),
%    io:format(" exists ~p~n",[{?MODULE,?LINE,TarBaseName}]),
    %% Store tar file	    
    ok=sd:call(IpAddr,Port,file,write_file,[TarBaseName,Binary],5000),
    ok=sd:call(IpAddr,Port,erl_tar,extract,[TarBaseName],5000),
    ok=sd:call(IpAddr,Port,delete,[TarBaseName],5000),
    Reply={'Loaded service ',ServiceStr,' with version ',Vsn,' on worker ',IpAddr,Port},
    Reply.




%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
compare_vsn(VsnLatest,ServiceStr,DirFile,Binary)->
    Time=erlang:system_time(),
    TempBaseName=integer_to_list(Time)++".app",
    ok=file:write_file(TempBaseName,Binary),
    {ok,[{application,ServiceAtom,AppInfo}]}=file:consult(TempBaseName),
  %  io:format("~p~n",[{?MODULE,?LINE,file:consult(TempBaseName)}]),
    {vsn,Vsn}=lists:keyfind(vsn,1,AppInfo),
    ok=file:delete(TempBaseName),
    Reply={cmp_vsn_strings(VsnLatest,Vsn),ServiceStr},
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


service_exists(ServiceStr,IpAddr,Port)->
    ServiceModule=list_to_atom(ServiceStr),
    case sd:call(IpAddr,Port,file,list_dir,["."],5000) of
	{ok,DirFiles}->
	    AppFiles= [find_app_file(DirFile,ServiceStr,IpAddr,Port) ||DirFile<-DirFiles],
	    case [{ServiceStr,AppFileName,Binary,IpAddr,Port} || {ok,ServiceStr,AppFileName,Binary,IpAddr,Port}<-AppFiles] of
		[]->
		    Reply=false;
		FoundAppFiles->
		    Reply=FoundAppFiles
	    end;
	{error,Err} ->
	    Reply={error,[?MODULE,?LINE,IpAddr,Port,ServiceStr,Err]}
    end,
    Reply.


find_app_file(DirFile,ServiceStr,IpAddr,Port)->
    AppFileName=filename:join([DirFile,"ebin",ServiceStr++".app"]),
    case sd:call(IpAddr,Port,file,read_file,[AppFileName],5000) of
	{error,Err}->
	    Reply={noent,DirFile,ServiceStr,IpAddr,Port};
	{ok,Binary}->
	    Reply={ok,ServiceStr,DirFile,Binary,IpAddr,Port}
    end,
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

cmp_vsn_strings(Vsn_A,Vsn_B)->
    % Vsn="2.42.120"
    % -> equal,A less B ,A larger B
    [P2Str_A,P1Str_A,P0Str_A]=string:tokens(Vsn_A,"."),
    [P2Str_B,P1Str_B,P0Str_B]=string:tokens(Vsn_B,"."),
    P2_A=list_to_integer(P2Str_A),
    P2_B=list_to_integer(P2Str_B),
    case {(P2_A<P2_B),(P2_A>P2_B)} of
	{false,false}->
	    P1_A=list_to_integer(P1Str_A),
	    P1_B=list_to_integer(P1Str_B),
	    case {(P1_A<P1_B),(P1_A>P1_B)} of
		{false,false}->
		    P0_A=list_to_integer(P0Str_A),
		    P0_B=list_to_integer(P0Str_B),
		    case {(P0_A<P0_B),(P0_A>P0_B)} of
			{false,false}->
			    Reply=equal;
			{true,false}->
			    Reply=less;
			{false,true} ->
			    Reply=larger
		    end;
		{true,false}->
		    Reply=less;
		{false,true} ->
		    Reply=larger
	    end;
	{true,false}->
	    Reply=less;
	{false,true} ->
	    Reply=larger
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

deploy_service(ServiceStr,IpAddr,Port) ->
 %   io:format("~p~n",[{?MODULE,?LINE,Worker,ServiceStr}]),
    case dets_dbase:read_object(vsn,{latest,ServiceStr}) of 
	{error,Err}->
%	    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
	    Reply={error,[IpAddr,Port,ServiceStr,Err]};
	VsnLatest ->
	    % Check if need to update  latest is newer then currant
	    ServiceModule=list_to_atom(ServiceStr),
	    LoadedApplications=sd:call(IpAddr,Port,application,loaded_applications,[],5000),
	    case lists:keyfind(ServiceModule,1,LoadedApplications) of
		false->
		    case load_test_service(ServiceStr,VsnLatest,IpAddr,Port,false) of
			{error,Err}->
%			    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
			    Reply={error,[IpAddr,Port,ServiceStr,Err]};
			ok->
			    Reply={ok,IpAddr,Port,ServiceStr,VsnLatest}
		    end;
		{ServiceModule,_AppInfo,Vsn}->
		    case cmp_vsn_strings(VsnLatest,Vsn) of
			larger->
			    case load_test_service(ServiceStr,VsnLatest,IpAddr,Port,true) of
				{error,Err}->
				    io:format("~p~n",[{?MODULE,?LINE,{error,Err}}]),
				    Reply={error,[IpAddr,Port,ServiceStr,Err]};
				ok->
				    Reply={ok,IpAddr,Port,ServiceStr,VsnLatest}
			    end;
			less ->
			    Reply={ok,IpAddr,Port,ServiceStr,Vsn};
			equal ->
			    Reply={ok,IpAddr,Port,ServiceStr,Vsn}
		    end
	    end
    end,
    Reply.

load_test_service(ServiceStr,VsnLatest,IpAddr,Port,false)->		    
    ok.
		    
remove_service(_ServiceStr,IpAddr,Port)->
    ok.
load_start_service(ServiceStr,IpAddr,Port)->
    


    ok.  
stop_unload_service(ServiceStr,IpAddr,Port)->
    ServiceModule=list_to_atom(ServiceStr),
    io:format("~p~n",[{?MODULE,?LINE,ServiceStr,sd:call(IpAddr,Port,application,stop,[ServiceModule],5000)}]),
    io:format("~p~n",[{?MODULE,?LINE,ServiceStr,sd:call(IpAddr,Port,application,unload,[ServiceModule],5000)}]),
    ok.
    

load_test_service(ServiceStr,IpAddr,Port)->
    VsnLatest=glurk,
    NewServiceDir=ServiceStr++"-"++VsnLatest,
 %   io:format("NewServiceDir = ~p~n",[{?MODULE,?LINE,NewServiceDir}]), 	 
    NewEbinPath=filename:join(NewServiceDir,"ebin"),
    true=sd:call(IpAddr,Port,code,add_patha,[NewEbinPath],5000),
 %   true=sd:call({Worker,brd_mgr},code,add_path,[NewEbinPath],5000),
    ServiceModule=glurk,
    io:format("NewDir = ~p~n",[{?MODULE,?LINE,sd:call(IpAddr,Port,code,lib_dir,[ServiceModule],5000)}]),
    {ok,NewBaseNames}= sd:call(IpAddr,Port,file,list_dir,[NewEbinPath],5000),
    load_files(NewBaseNames,{IpAddr,Port,brd_mgr},NewEbinPath),

    io:format("~p~n",[{?MODULE,?LINE,sd:call(IpAddr,Port,application,load,[ServiceModule],5000)}]),
    io:format("~p~n",[{?MODULE,?LINE,sd:call(IpAddr,Port,application,start,[ServiceModule],5000)}]),
    case sd:call(IpAddr,Port,ServiceModule,test,[],5000) of
	ok->
	    Reply=ok;
	Err ->
	    % Remove NewService
	  %  io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,stop,[ServiceModule],5000)}]),
	  %  io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,unload,[ServiceModule],5000)}]),
	  %  purge_files(NewBaseNames,{Worker,brd_mgr},NewEbinPath),
	  %  sd:call({Worker,brd_mgr},code,del_path,[NewServiceDir],5000),
	    
	  %  true=sd:call({Worker,brd_mgr},code,add_patha,[EbinCurrentServicePath],5000),
	  %  {ok,NewBaseNames}= sd:call({Worker,brd_mgr},file,list_dir,[NewEbinPath],5000),
	  %  load_files(NewBaseNames,{Worker,brd_mgr},NewEbinPath),
	    
	  %  io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,load,[ServiceModule],5000)}]),
	  %  io:format("~p~n",[{?MODULE,?LINE,sd:call({Worker,brd_mgr},application,start,[ServiceModule],5000)}]),		
	%	true=sd:call({Worker,brd_mgr},os,cmd,["rm -r "++ NewServiceDir],5000),
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.   


	    
%	    CurrentServiceDir=filename:basename(CurrentPath),	   
%	    io:format("CurrentServiceDir = ~p~n",[{?MODULE,?LINE,CurrentServiceDir}]), 	    	    
%	    EbinCurrentServicePath=filename:join(CurrentPath,"ebin"),
%	    io:format("~p~n",[{?MODULE,?LINE,EbinCurrentServicePath}]),
%	    {ok,CurrentBaseNames}= sd:call({Worker,brd_mgr},file,list_dir,[EbinCurrentServicePath],5000),
%	    io:format("CurrentBaseNames = ~p~n",[{?MODULE,?LINE,CurrentBaseNames}]), 	 
%	    purge_files(CurrentBaseNames,{Worker,brd_mgr},EbinCurrentServicePath),
%	    sd:call({Worker,brd_mgr},code,del_path,[CurrentServiceDir],5000)
 %   end,
    
  

load_files([],_,_)->
  %  io:format("load files ~p~n",[{?MODULE,?LINE,[]}]),
    ok;
load_files([BaseName|T],Resource,EbinCurrentServicePath) ->
    _RelativeFileName=filename:join(EbinCurrentServicePath,BaseName),
    case filename:extension(BaseName) of
	".beam"->
	    ModuleStr=filename:basename(BaseName,".beam"),
	    Module=list_to_atom(ModuleStr),
%	    io:format("load Module ~p~n",[{?MODULE,?LINE,Module}]),
	    {module,Module}=sd:call(Resource,code,load_file,[Module],5000);
	_ ->
	    do_nothing
    end,
    load_files(T,Resource,EbinCurrentServicePath).

purge_files([],_,_)->
 %   io:format("Purge  Module ~p~n",[{?MODULE,?LINE,[]}]),
    ok;
purge_files([BaseName|T],Resource,EbinCurrentServicePath) ->
  %  io:format("Purge  Module ~p~n",[{?MODULE,?LINE,BaseName,EbinCurrentServicePath}]),
    _RelativeFileName=filename:join(EbinCurrentServicePath,BaseName),
    case filename:extension(BaseName) of
	".beam"->
	    ModuleStr=filename:basename(BaseName,".beam"),
	    Module=list_to_atom(ModuleStr),
%	    io:format("Purge  Module ~p~n",[{?MODULE,?LINE,Module}]),
	    sd:call(Resource,code,purge,[Module],5000);
	Err->
	    io:format("do nothing ~p~n",[{?MODULE,?LINE,Err}]),
	    do_nothing
    end,
    purge_files(T,Resource,EbinCurrentServicePath).



connect_worker(Worker,DbaseFile,_Glurk) ->
    [{brd_ipaddr,IpAddr},
     {brd_port,BrdPort},
     {location,_},
     {capability,_},
     {appl,_}]=dets_dbase:read_object(object,{worker,Worker},DbaseFile),
    true=sd:add_resource({Worker,brd_mgr},IpAddr,BrdPort),
 %   io:format("~p~n",[{?MODULE,?LINE,Worker}]),
    case sd:call({Worker,brd_mgr},brd_mgr,connect,[],5000) of
	active->
%	    io:format("~p~n",[{?MODULE,?LINE,Worker}]),
	    Reply={active,Worker,[]};   
	standby ->
	   sd:delete_resource({Worker,brd_mgr},IpAddr,BrdPort),
%	    io:format("~p~n",[{?MODULE,?LINE,Worker}]),
	    Reply={standby,Worker,[]};  
	{error,Err}->
	    sd:delete_resource({Worker,brd_mgr},IpAddr,BrdPort),
%	    io:format("~p~n",[{?MODULE,?LINE,Worker,error,Err}]),
	    Reply={error,Worker,[Err]};   
	Err ->
	    sd:delete_resource({Worker,brd_mgr},IpAddr,BrdPort),
%	    io:format("~p~n",[{?MODULE,?LINE,Worker,Err}]),
	    Reply={error,Worker,[Err]}   
    end,				
 %   io:format("~p~n",[{?MODULE,?LINE}]),  
    Reply.



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% active = services to run
%% canditate = temporary dir where new load is loaded and teast befor update
%% rollback = active -> rollback when updating
%% --------------------------------------------------------------------
load_services(latest_release,IpAddr,Port)->
    %% Preventive action stop candidate vm and  delete candidate dir
    Host=sd:call(IpAddr,Port,net_adm,localhost,[],5000),
    CandidateNodeStr="candidate@"++Host,
    CandidateNode=list_to_atom(CandidateNodeStr),
    sd:call(IpAddr,Port,rpc,call,[CandidateNode,erlang,halt,[]],5000),
    sd:call(IpAddr,Port,os,cmd,["rm -r candidate"],5000),
    timer:sleep(500),
    
    %% Create candidate dir and  services dirs
    
    CandidateDir="candidate",
    CandidateNodeName="candidate",
    case start_node(IpAddr,Port,CandidateDir,CandidateNodeName) of
	{ok,CandidateNode}->
	    ok=sd:call(IpAddr,Port,file,make_dir,[CandidateDir],5000),
	    MandatoryServices=dets_dbase:read_object(services,{deployment,"mandatory"}),
	    ResultMandatory=store_test_tar_files(MandatoryServices,IpAddr,Port,CandidateDir,CandidateNode,[]),
	  %  io:format("~p~n",[{?MODULE,?LINE,time(),self(),ResultMandatory}]),	   
	    WorkerServices=dets_dbase:read_object(services,{deployment,IpAddr,Port}),
	    ResultWorkers=store_test_tar_files(WorkerServices,IpAddr,Port,CandidateDir,CandidateNode,[]),
	 %   io:format("~p~n",[{?MODULE,?LINE,time(),self(),ResultWorkers}]),	
	  %  ok=stop_node( sd:call(Worker,rpc,call,[CandidateNode,erlang,halt,[]],5000),
	    case {ResultMandatory,ResultWorkers} of
		{ok,ok}->
		    Reply=ok;
		_->
		    Reply={error,{?MODULE,?LINE,ResultMandatory,ResultWorkers}}
	    end,
	    sd:call(IpAddr,Port,rpc,call,[CandidateNode,erlang,halt,[]],5000);
	{error,Err}->
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.

store_test_tar_files([],_IpAddr,_Port,_CandidateDir,_CandidateNode,Acc)->
    Acc;
store_test_tar_files([{Service,_NumInstances}|T],IpAddr,Port,CandidateDir,CandidateNode,_Acc)->
    Key={service,Service},
    case dets_dbase:read_object(latest_release,Key) of
	{error,Err}->
	    NewAcc={error,{?MODULE,?LINE,Err}},
	    NewT=[];
	LatestStr->
	    {TarFileName,Binary}=dets_dbase:read_object(LatestStr,Key),
	    ServiceDir=filename:join(CandidateDir,Service),    
	    case sd:call(IpAddr,Port,file,make_dir,[ServiceDir],5000) of
		ok->
		    case sd:call(IpAddr,Port,file,write_file,[TarFileName,Binary],5000) of
			ok->
			    TarCmd="tar -xvf "++TarFileName++" -C "++ServiceDir,
			    sd:call(IpAddr,Port,os,cmd,[TarCmd],5000),
			   % timer:sleep(1000),
			    Ebin=filename:join(ServiceDir,"ebin"),
		%	     io:format("~p~n",[{?MODULE,?LINE,time(),self(),sd:call(Worker,rpc,call,[CandidateNode,code,add_patha,[Ebin]],5000)}]),
			    sd:call(IpAddr,Port,rpc,call,[CandidateNode,code,add_patha,[Ebin]],5000),
			    case test_service(Service,CandidateNode,IpAddr,Port) of
				ok->
				    NewAcc=ok,
				    NewT=T;
				{error,Err}->
				    NewAcc={error,{?MODULE,?LINE,Err}},
				    NewT=[]
			    end;
			{error,Err}->
			    NewAcc={error,{?MODULE,?LINE,Err}},
			    NewT=[]
		    end;
		{error,eexist} ->
		    case sd:call(IpAddr,Port,file,write_file,[TarFileName,Binary],5000) of
			ok->
			    TarCmd="tar -xvf "++TarFileName++" -C "++ServiceDir,
			    sd:call(IpAddr,Port,os,cmd,[TarCmd],5000),
			    %timer:sleep(1000),
			    _Ebin=filename:join(ServiceDir,"ebin"),
%			    io:format("~p~n",[{?MODULE,?LINE,sd:call(Worker,rpc,call,[CandidateNode,code,add_patha,[Ebin]],5000)}]),
			    case test_service(Service,CandidateNode,IpAddr,Port) of
				ok->
				    NewAcc=ok,
				    NewT=T;
				{error,Err}->
				    NewAcc={error,{?MODULE,?LINE,Err}},
				    NewT=[]
			    end;
			{error,Err}->
			    NewAcc={error,{?MODULE,?LINE,Err}},
			    NewT=[]
		    end;
		Err->
		    NewAcc={error,{?MODULE,?LINE,Err}},
		    NewT=[]
	    end
    end,    
    store_test_tar_files(NewT,IpAddr,Port,CandidateDir,CandidateNode,NewAcc).

start_node(IpAddr,Port,Dir,NodeName)->
 %   io:format("~p~n",[{?MODULE,?LINE,sd:call(Worker,file,get_cwd,[],5000)}]),
    
    EbinDirs=filename:join([Dir,"*","ebin"]),
%    io:format("~p~n",[{?MODULE,?LINE,EbinDirs}]),
    Host=sd:call(IpAddr,Port,net_adm,localhost,[],5000),
    NodeStr=NodeName++"@"++Host,
    Node=list_to_atom(NodeStr),
    ErlCmd="erl -pa "++EbinDirs++" "++"-sname "++NodeStr++" "++"-detached",
    
    case sd:call(IpAddr,Port,os,cmd,[ErlCmd],5000) of
	[]->
	    Reply={ok,Node},
	    timer:sleep(3000); % Wait for Linux to start the VM
	Err->

	    Reply={error,{?MODULE,?LINE,couldnt_start_node,Err}}
    end,
    Reply.


test_service(ServiceStr,Node,IpAddr,Port) ->
    Service=list_to_atom(ServiceStr),
    _Ping=sd:call(IpAddr,Port,net_adm,ping,[Node],5000),
    _Path=sd:call(IpAddr,Port,rpc,call,[Node,code,get_path,[]],5000),
    R1=sd:call(IpAddr,Port,rpc,call,[Node,application,load,[Service]],5000),
    R2=sd:call(IpAddr,Port,rpc,call,[Node,application,start,[Service]],5000),
    R3=sd:call(IpAddr,Port,rpc,call,[Node,Service,test,[]],5000),
    case {R1,R2,R3} of
	{ok,ok,ok}->
	    Reply=ok;
	 _->
	    Reply={error,{?MODULE,?LINE,Service,R1,R2,R3}}
    end,
    Reply.




%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
  
test_services([],IpAddr,Port,_Node,ok)->
    ok;
test_services([],IpAddr,Port,_Node,{error,Service})->
    {error,Service};
test_services(_ListServicesStr,IpAddr,Port,_Node,{error,Service}) ->
    {error,Service};
test_services([ServiceStr|T],IpAddr,Port,Node,_Acc) ->
    io:format("Testing service ~p~n",[{IpAddr,Port,ServiceStr}]),
    Service=list_to_atom(ServiceStr),
    R1=sd:call(IpAddr,Port,rpc,call,[Node,application,load,[Service]],5000),
    R2=sd:call(IpAddr,Port,rpc,call,[Node,application,start,[Service]],5000),
    R3=sd:call(IpAddr,Port,rpc,call,[Node,Service,test,[]],5000),
    case {R1,R2,R3} of
	{ok,ok,ok}->
	    NewAcc=ok;
	_ ->
	    NewAcc={error,Service}
    end,
    test_services(T,IpAddr,Port,Node,NewAcc).
    
		     

	    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
