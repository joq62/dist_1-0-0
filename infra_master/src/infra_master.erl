%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(infra_master).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
-include("include/infra.hrl").
%% --------------------------------------------------------------------
-define(POLL_TIME,60*1000).
-define(CONFIGFILE,"service.config").

%% External exports Support functions


%-export([have_capability/2,have_capabilities/2]).
-export([start_infra/0,stop_infra/0]).

%% External exports Gen server

% Services to appl_master
-export([get_appfile/2,store_appfile/3,
	 deploy_service/2
	 ]).


-export([deploy_service/3,service_deployed/2,delete_deployed/3,
	 start_service/3,stop_service/3,
	 test_service/2,
	 make_service_available/3,
	 candidate_workers/1]).

% Administrate workers

%-export([add_worker/2,delete_worker/1,update_worker/2,read_info/1]).

-export([add_worker/4,delete_worker/2,
	 connect_workers/2,connect/4,
	 get_active/0,get_disconnected/0,get_allworkers/0,

	 worker_info/2,
	 get_candidate_workers/1,
	 release_worker/1,
	 active_workers/0,passive_workers/0,
	 find_worker/1]).

-export([start/0,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {active,disconnected,allWorkers}).
%% ====================================================================
%% External functions
%% ====================================================================

start_infra()->
    Load=application:load(?MODULE),
    Start=application:start(?MODULE),
    {Load,Start}.

stop_infra()->
    ok=application:stop(infra_master),
    ok=application:unload(infra_master),
    ok.


%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%% New functions 2018-04-06 ---------------------------------------

deploy_service(AppBaseName,Binary)->
    gen_server:call(?MODULE, {deploy_service,AppBaseName,Binary},infinity). 

store_appfile(AppBaseName,VsnAppFile,Binary)->
    gen_server:call(?MODULE, {store_appfile,AppBaseName,VsnAppFile,Binary},infinity). 

get_appfile(AppBaseName,VsnAppFile)->
    gen_server:call(?MODULE, {get_appfile,AppBaseName,VsnAppFile},infinity). 


%% ----------------------------------------------------------------

candidate_workers(NeededCapabilities)->
    gen_server:call(?MODULE, {candidate_workers,NeededCapabilities},infinity).

make_service_available(ServiceStr,IpAddr,Port)->
    gen_server:cast(?MODULE,{make_service_available,ServiceStr,IpAddr,Port}).
		      
test_service(ServiceStr,WorkerId)->
      gen_server:call(?MODULE, {test_service,ServiceStr,WorkerId},infinity).  

start_service(ServiceStr,Vsn,WorkerId)->
      gen_server:call(?MODULE, {start_service,ServiceStr,Vsn,WorkerId},infinity).    

stop_service(ServiceStr,Vsn,WorkerId)->
      gen_server:call(?MODULE, {stop_service,ServiceStr,Vsn,WorkerId},infinity).    

delete_deployed(ServiceStr,Vsn,WorkerId)->
    gen_server:call(?MODULE, {delete_deployed,ServiceStr,Vsn,WorkerId},infinity).   
deploy_service(ServiceStr,Vsn,WorkerId)->
        gen_server:call(?MODULE, {deploy_service,ServiceStr,Vsn,WorkerId},infinity).    
service_deployed(ServiceStr,WorkerId)->
        gen_server:call(?MODULE, {service_deployed,ServiceStr,WorkerId},infinity).


%% Final :-)
add_worker(IpAddr,Port,Location,Capabilities)->
     gen_server:call(?MODULE, {add_worker,IpAddr,Port,Location,Capabilities},infinity).

delete_worker(IpAddr,Port)->
         gen_server:call(?MODULE, {delete_worker,IpAddr,Port},infinity).
%%%%

connect_workers(Interval,DbaseFile)->
        gen_server:cast(?MODULE, {connect_workers,Interval,DbaseFile}).
% 
connect(IdBrd,IpAddrWorker,PortBrd,PortWorker)->
    gen_server:call(?MODULE, {connect,IdBrd,IpAddrWorker,PortBrd,PortWorker},infinity).

get_active()->
        gen_server:call(?MODULE, {get_active},infinity).
get_disconnected()->
        gen_server:call(?MODULE, {get_disconnected},infinity).
get_allworkers()->
        gen_server:call(?MODULE, {get_allworkers},infinity).

%%%%%%%%%%%

worker_info(WorkerId,Info)->
     gen_server:call(?MODULE,{worker_info,WorkerId,Info},infinity).

get_candidate_workers(NeededCapabilities)->
    gen_server:call(?MODULE, {get_candidate_workers,NeededCapabilities},infinity).
    
find_worker(WorkerId)->
    gen_server:call(?MODULE, {find_worker,WorkerId},infinity).

active_workers()->
    gen_server:call(?MODULE, {active_workers},infinity).

passive_workers()->
    gen_server:call(?MODULE, {passive_workers},infinity).

%connected_workers()->
 %   gen_server:call(?MODULE, {connected_workers},infinity).

release_worker(WorkerId)->
    gen_server:cast(?MODULE, {release_worker,WorkerId}).


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
% dict:fetch(oam_rpi3,D1).
% [{brd_ip_port,"80.216.90.159"},
% {port,6001},
% {worker_ip_port,"80.216.90.159"},
%  {port,6002}]
%
%% --------------------------------------------------------------------
init([]) ->
    {ok,ConfigInfo}=file:consult(?CONFIGFILE),
    {{infra_master,initial_file},InitialFile}=lists:keyfind({infra_master,initial_file},1,ConfigInfo),
    {{infra_master,hearbeat_interval},HeartBeatInterval}=lists:keyfind({infra_master,hearbeat_interval},1,ConfigInfo),
	    
    % Intitiate local dets_dbase with name ?MODULE 
    case filelib:is_file(?MODULE) of
	true->
	    already_createded;
	false->
	    {ok,_}=dets_dbase:create(?MODULE,[{type,set}]),
%	    {ok,InitialValues}=file:consult(InitialFile),
%	    {ok,R}=dets:open_file(?MODULE),
%	    true=dets:insert_new(R,InitialValues),
%	    ok=dets:close(R),
	    {ok,WorkersInfo}=file:consult("worker_spec/workers.spec"),
	    [dets_dbase:create_object({brd_id,BrdId},WorkerInfo,?MODULE)||{{brd_id,BrdId},WorkerInfo}<-WorkersInfo]
    end,

    % Get all registered workers
    L=dets_dbase:match({{brd_id,'_'},'$3'},?MODULE),
    AllWorkers=[Z||[Z]<-L],
    % Connect to active workers
    ConnectTries=[{net_adm:ping(list_to_atom(BrdId)),BrdId}||[{brd_id,BrdId},
							      {location,_Zone},
							      {capability,_Capability},
							      {appl,_Appl}]<-AllWorkers],
    Active=[BrdId||{pong,BrdId}<-ConnectTries],
    Disconnected=[BrdId||{pang,BrdId}<-ConnectTries],
    
    
    % Start periodic check of nodes
 %   spawn(infra_master_lib,poll_workers,[start,30000,?MODULE]),    
    {ok, #state{active=Active,disconnected=Disconnected}}.   

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
%%   Pre requisite: Appfiles must be stored in dets dbase before using deploy_service
%%   use store_app_file(AppFileName,Bi
%%   2. 
%% Returns: non
%% --------------------------------------------------------------------
handle_call({store_appfile,AppBaseName,VsnAppFile,Binary},_From, State) ->
    Reply=infra_master_lib:store_appfile(AppBaseName,VsnAppFile,Binary,?MODULE),
    {reply, Reply, State};

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%%   Pre requisite: Appfiles must be stored in dets dbase before using deploy_service
%%   use store_app_file(AppFileName,Bi
%%   2. 
%% Returns: non
%% --------------------------------------------------------------------
handle_call({get_appfile,AppFileName,VsnAppFile},_From, State) ->
    Reply=infra_master_lib:get_appfile(AppFileName,VsnAppFile,?MODULE),
    {reply, Reply, State};


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%%   Pre requisite: Appfiles must be stored in dets dbase before using deploy_service
%%   use store_app_file(AppFileName,Binary) 
%%   1. get appfile, read AppInfo, delete Appfile
%%   2. get tar file and store in remote board
%%   3. start new node - nodename service-vsn@"localhost"
%%   2. load services - and test 
%%     2.1 check if services already are loaded
%%   3
%% Returns: non
%% --------------------------------------------------------------------

handle_call({deploy_service,AppBaseName,VsnAppFile},_From, State) ->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,?MODULE),
    ok=file:write_file(AppBaseName,Binary),
    {ok,AppInfo}=file:consult(AppBaseName),
    ok=file:delete(AppBaseName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {needed_services,NeededServiceAppSpecs}=lists:keyfind(needed_services,1,AppInfo),

    

    {service,ServiceStr}=lists:keyfind(service,1,MainServiceInfo),
    {vsn,Vsn}=lists:keyfind(vsn,1,MainServiceInfo),
    {board_type,BrdType}=lists:keyfind(board_type,1,MainServiceInfo),
    {numInstances,NumInstances}=lists:keyfind(numInstances,1,MainServiceInfo),
    {geo,Zone}=lists:keyfind(geo,1,MainServiceInfo),
    
    Reply={MainServiceInfo,NeededServiceAppSpecs},
    
    {reply, Reply, State};

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%  [{node_id,'test_app_pool@joq-desktop'},{location,zone_0},{capability,[]},{appl,[]}]
%   
%
handle_call({add_worker,NodeId,Location,Capabilities},_From, State) ->    
    case dets_dbase:member_object({node_id,NodeId},?MODULE) of
	true->
	    Reply={error,[?MODULE,?LINE,'allredy exists ',NodeId]};
	false->
	    case dets_dbase:create_object({node_id,NodeId},
					  [{node_id,NodeId},
					   {location,Location},
					   {capability,Capabilities},{appl,[]}],?MODULE) of
		{error,Err}->
		    Reply={error,[?MODULE,?LINE,Err]};
		{ok,store}->
		    Reply=ok
	    end
    end,	    
    {reply, Reply, State};

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%  [{addr,"localhost",10501},{location,zone_0},{capability,[]},{appl,[]}]
%   
%
handle_call({delete_worker,NodeId},_From, State) ->    
    case dets_dbase:member_object({node_id,NodeId},?MODULE) of
	false->
	    Reply={error,[?MODULE,?LINE,'not exits ',NodeId]};
	true->
	    case dets_dbase:delete_object({node_id,NodeId},?MODULE) of
		{error,Err}->
		    Reply={error,[?MODULE,?LINE,Err]};
		{ok,_}->
		    Reply=ok
	    end
    end,	    
    {reply, Reply, State};

% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
% _IpAddrWorker not used already stored in config file - later release can use it

handle_call({candidate_workers,NeededCapabilities},_From, State) ->
    {active,ActiveWorkers}=State#state.active,
    case NeededCapabilities of
	[]->
	    Reply=[NodeId||[{node_id,NodeId},
				      {location,Location},
				      {capability,Capabilities},
				      {appl,Apps}]<-ActiveWorkers];
	_->
	    Reply=infra_master_lib:candidate_workers(NeededCapabilities,ActiveWorkers)
    end,
    {reply, Reply, State};

% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
% _IpAddrWorker not used already stored in config file - later release can use it
handle_call({load_service,ServiceStr,Vsn,WorkerId},_From, State) ->
    {active,ActiveWorkers}=State#state.active,
    case lists:member(WorkerId,ActiveWorkers) of
	false-> %Worker not active
	     Reply={error,{?MODULE,?LINE,[WorkerId,not_active]}};
	true->
	    case infra_master_lib:service_deployed(ServiceStr,WorkerId) of
		false-> % Service does not exists
		    Reply=infra_master_lib:load_service(ServiceStr,Vsn,WorkerId,?MODULE);
		{true,Deployed}->  %[{Dir,ServiceStr,Vsn}]
		    case lists:keyfind(Vsn,3,Deployed) of
			false-> % Earlier versions of services are deployed
			    Reply=infra_master_lib:load_service(ServiceStr,Vsn,WorkerId);
			{Dir,ServiceStr,Vsn}-> % service is deployed 
			    Reply={error,{?MODULE,?LINE,[ServiceStr,already_deployed]}}
		    end
	    end
    end,
    {reply, Reply, State};

%%???????????????????????????????????????????????????????????????????????????????????????????++

handle_call({test_service,ServiceStr,WorkerId},_From, State) ->
    Reply=infra_master_lib:test_service(ServiceStr,WorkerId),
    {reply, Reply, State};
handle_call({start_service,ServiceStr,Vsn,WorkerId},_From, State) ->
    Reply=infra_master_lib:start_service(ServiceStr,Vsn,WorkerId),
    {reply, Reply, State};

handle_call({stop_service,ServiceStr,Vsn,WorkerId},_From, State) ->
    Reply=infra_master_lib:stop_service(ServiceStr,Vsn,WorkerId),
    {reply, Reply, State};

handle_call({delete_deployed,ServiceStr,Vsn,WorkerId},_From, State) ->
    Reply=infra_master_lib:delete_deployed(ServiceStr,Vsn,WorkerId),
    {reply, Reply, State};

handle_call({deploy_service,ServiceStr,Vsn,WorkerId},_From, State) ->
    {active,ActiveWorkers}=State#state.active,
    case lists:member(WorkerId,ActiveWorkers) of
	false-> %Worker not active
	     Reply={error,{?MODULE,?LINE,[WorkerId,not_active]}};
	true->
	    case infra_master_lib:service_deployed(ServiceStr,WorkerId) of
		false-> % Service does not exists
		    Reply=infra_master_lib:load_service(ServiceStr,Vsn,WorkerId,?MODULE);
		{true,Deployed}->  %[{Dir,ServiceStr,Vsn}]
		    case lists:keyfind(Vsn,3,Deployed) of
			false-> % Earlier versions of services are deployed
			    Reply=infra_master_lib:load_service(ServiceStr,Vsn,WorkerId);
			{Dir,ServiceStr,Vsn}-> % service is deployed 
			    Reply={error,{?MODULE,?LINE,[ServiceStr,already_deployed]}}
		    end
	    end
    end,
    {reply, Reply, State};

handle_call({service_deployed,ServiceStr,WorkerId},_From, State) ->
    Reply=infra_master_lib:service_deployed(ServiceStr,WorkerId),
    {reply, Reply, State};


handle_call({connect,IdBrd,_IpAddrWorker,_PortBrd,_PortWorker},_From, State) -> 
    case dict:is_key(IdBrd,State#state.allWorkers) of
	true->
	    WorkerInfo=dict:fetch(IdBrd,State#state.allWorkers),
	    NewDictActive=dict:store(IdBrd,WorkerInfo,State#state.active),
	    Reply=ok,
	    NewState=State#state{active=NewDictActive};
	false->
	    Reply={error,{?MODULE,?LINE,unknown_brdid,IdBrd}},
	    NewState=State
    end,
    {reply, Reply, NewState};


handle_call({get_active},_From, State) ->
    Reply=State#state.active,
    {reply, Reply, State};
handle_call({get_disconnected},_From, State) ->
    Reply=State#state.disconnected,
    {reply, Reply, State};

handle_call({get_allworkers},_From, State) ->
    L=dets_dbase:match({{brd_id,'_'},'$3'},?MODULE),
    Reply=[Z||[Z]<-L],
    {reply, Reply, State};

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
% key={worker,"w0"}
% value=[{brd_ipaddr,"localhost"},{brd_port,10501},{location,stan_joarum},{capability,[dbase]},{appl,[]}]},

handle_cast({make_service_available,ServiceStr,IpAddr,Port}, State) ->
    sd:add_resource(ServiceStr,IpAddr,Port), % Add to infra master
    {active,ActiveWorkers}=State#state.active, 
    [{WorkerId,sd:call({WorkerId,brd_mgr},sd,add_resource,[ServiceStr,IpAddr,Port],5000)}||WorkerId<-ActiveWorkers],
    {noreply, State};
		
%(dbase,IpAddrDbase,PortDbase)

handle_cast({connect_workers,Interval,DbaseFile}, State) ->
%    io:format("CONNECT WORKERS = ~p~n",[{?MODULE,?LINE}]),
    Result=infra_master_lib:connect_workers(DbaseFile),
    NewState=State#state{active=lists:keyfind(active,1,Result),
			 disconnected=lists:keyfind(disconnected,1,Result)},


   % infra_master_lib:add_resorces_sd(ActiveWorkers),
   % infra_master_lib:delete_resources_sd(DisConnectedWorkers),
    
    io:format("time = = ~p~n",[time()]),
    io:format("active = ~p~n",[lists:keyfind(active,1,Result)]),
    io:format("standby = ~p~n",[lists:keyfind(standby,1,Result)]),
    io:format("disconnected = ~p~n",[lists:keyfind(disconnected,1,Result)]),
 %   io:format("loaded_services = ~p~n",[lists:keyfind(loaded_services,1,Result)]),
 %   io:format("ok_services = ~p~n",[lists:keyfind(ok_services,1,Result)]),
 %   io:format("failed_Services = ~p~n",[lists:keyfind(failed_services,1,Result)]),
 %   io:format("updated_services, = ~p~n",[lists:keyfind(updated_services,1,Result)]),
 %   io:format("tested_services, = ~p~n",[lists:keyfind(tested_services,1,Result)]),
    spawn(infra_master_lib,poll_workers,[Interval,DbaseFile]),
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_info(Info, State) ->
    io:format("unmatched match cast ~p~n",[{time(),?MODULE,?LINE,Info}]),
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


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
    

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
service_exists(ServiceStr,WorkerId)->
    case sd:call(dbase,infra_dbase,read_object,[vsn,{latest,ServiceStr}],5000) of 
	{error,Err}->
	    Reply={error,[WorkerId,ServiceStr,Err]};
	VsnLatest ->
	    LatestDirName=ServiceStr++"-"++VsnLatest,
	    case infra_master_lib:service_deployed(ServiceStr,WorkerId) of
		false->
		    Reply=false;
		{error,Err}->
		    Reply=false;
		[{ServiceStr,LatestDirName,_TarFile,WorkerId}] ->
		    Reply=true;
		[{ServiceStr,_DeployedDirName,_TarFile,WorkerId}] ->
		    Reply=false;
		Glurk ->
		    Reply=Glurk
	    end
    end,
    Reply.
