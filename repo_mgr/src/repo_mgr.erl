%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(repo_mgr).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(POLL_TIME,2000).
-define(CONFIGFILE,"service.config").

%-export([have_capability/2,have_capabilities/2]).
-export([test/0,start_repo/0,stop_repo/0]).

%% External exports Gen server
-export([all_services/0,
	 get_vsns/1,
	 build_release/2,
	 create_update_service/2,update_service/2,read_service/2,delete_service/2,
	 latest_release/1,store_service/3,remove_service/2,get_service/2,get_service_releases/1]).

-export([start/0,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start_repo()->
    Load=application:load(?MODULE),
    Start=application:start(?MODULE),
    {Load,Start}.

stop_repo()->
    Stop=application:stop(?MODULE),
    Unload=application:unload(?MODULE),
    {Stop,Unload}.

test()->
    ok.
    

%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).




%--------------------------------------------------------- 


get_vsns(ServiceStr)->
        gen_server:call(?MODULE, {get_vsns,ServiceStr},infinity).

build_release(Release,DevAreaDir)->
    gen_server:call(?MODULE, {build_release,Release,DevAreaDir},infinity).

create_update_service(ServiceDir,DevAreaDir)->
    gen_server:call(?MODULE, {create_update_service,ServiceDir,DevAreaDir},infinity).
update_service(ServiceDir,DevAreaDir)->
    gen_server:call(?MODULE, {update_service,ServiceDir,DevAreaDir},infinity).
read_service(Item,Service)->
    gen_server:call(?MODULE, {read_service,Item,Service},infinity).
delete_service(Service,Vsn)->
    gen_server:call(?MODULE, {delete_service,Service,Vsn},infinity).

latest_release(ServiceStr)->
    gen_server:call(?MODULE, {latest_release,ServiceStr},infinity).
store_service(ServiceStr,ReleaseStr,RepoDir)->
    gen_server:call(?MODULE, {store_service,ServiceStr,ReleaseStr,RepoDir},infinity).
remove_service(ServiceStr,ReleaseStr)->
    gen_server:call(?MODULE, {remove_service,ServiceStr,ReleaseStr},infinity).


%%------------------------
all_services()->
         gen_server:call(?MODULE, {all_services},infinity).
get_service(ServiceStr,ReleaseStr)->
     gen_server:call(?MODULE, {get_service,ServiceStr,ReleaseStr},infinity).
get_service_releases(ServiceStr)->
     gen_server:call(?MODULE, {get_service_releases,ServiceStr},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}     
%  Dat Structure
%% --------------------------------------------------------------------
init([]) ->
    {ok,_}=dets_dbase:create(?MODULE,[{type,set}]),
    io:format("Application Started ~p~n",[{?MODULE}]),

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

handle_call({all_services},_From, State) ->
    Reply=dets_dbase:all(?MODULE),
    {reply, Reply, State};
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

handle_call({get_vsns,ServiceStr},_From, State) ->
    Vsns=dets_dbase:match({{service,ServiceStr,'$1'},'_'},?MODULE),
    Reply=[Vsn||[Vsn]<-Vsns],
    {reply, Reply, State};

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

handle_call({build_release,Release,DevAreaDir},_From, State) ->
    Reply=repo_mgr_lib:build_release(Release,DevAreaDir,?MODULE),
    {reply, Reply, State};

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
handle_call({delete_service,Service,Vsn},_From, State) ->
    %check if there exist vsn < Vsn the update latest with that
    Reply=repo_mgr_lib:delete_service(Service,Vsn,?MODULE),
    {reply, Reply, State};

handle_call({create_update_service,ServiceDir,DevAreaDir},_From, State) ->
    Reply=repo_mgr_lib:create_update_service(ServiceDir,DevAreaDir,?MODULE),
    {reply, Reply, State};

handle_call({update_service,ServiceDir,DevAreaDir},_From, State) ->
    Reply=repo_mgr_lib:update_service(ServiceDir,DevAreaDir,?MODULE),
    {reply, Reply, State};

handle_call({read_service,Item,Service},_From, State) ->
    Reply=repo_mgr_lib:read_service(Item,Service,?MODULE),
    {reply, Reply, State};

% --------------------------------------------------------------------
%% Function: store release 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
handle_call({latest_release,ServiceStr},_From, State) ->
    KeyLatest={latest,ServiceStr},
    Reply=dets_dbase:read_object(vsn,KeyLatest,?MODULE),
    {reply, Reply, State};

handle_call({get_service_releases,Service},_From, State) ->
    Reply=sd:call(dbase,infra_dbase,get_service_releases,[Service],5000),
    {reply, Reply, State};

handle_call({get_service,ServiceStr,ReleaseStr},_From, State) ->
    KeyServiceData={service,ServiceStr,ReleaseStr},
    case dets_dbase:get(KeyServiceData,?MODULE) of
	{ok,[]}->
	    Reply={error,[?MODULE,?LINE,'doesnt exists ',ServiceStr,ReleaseStr]};
	{ok,[{{service,ServiceStr,ReleaseStr},[{release,[TarBaseName,Binary]}]}]}->
	    Reply={ok,TarBaseName,Binary}
    end,
    {reply, Reply, State};

handle_call({remove_service,Service,ReleaseStr},_From, State) ->
    KeyServiceData={ReleaseStr,Service},
    Reply=dets_dbase:remove(KeyServiceData,?MODULE),
    {reply, Reply, State};

% --------------------------------------------------------------------
%% Function: store service
%% Description:
%% Returns: non
%% Dbase structure
%% service_info:
%% Key= {service,release}
%% Value={TarFileName,TarFile}
%% --------------------------------------------------------------------
handle_call({store_service,Service,ReleaseStr,RepoDir},_From, State) ->
    Reply=repo_mgr_lib:store_service(Service,ReleaseStr,RepoDir),
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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
