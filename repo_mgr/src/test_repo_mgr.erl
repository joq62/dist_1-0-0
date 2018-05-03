%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_repo_mgr).

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
start_test()->
  % file:delete(repo_mgr), 
    {ok,ok}=repo_mgr:start_repo(),
    ok.

build_release_ok_100_test()->
    DevAreaDir="../dev",
    Release="1.0.0",
%    {ok,store}=rpc:call(node(),repo_mgr,build_release,[Release,DevAreaDir],5000),
    {ok,store}=rpc:call(node(),repo_mgr,create_update_service,["myadd","../dev"],5000),
    ok.

extract_test()->
    {ok,"myadd-3.0.0.tar",Binary}=rpc:call(node(),repo_mgr,get_service,["myadd","3.0.0"],5000),
    ok=file:write_file("myadd-3.0.0.tar",Binary),
    ok=erl_tar:extract("myadd-3.0.0.tar"),

    ok.
    
repo_all_services_test_xx()->
    {ok,AllServices}=rpc:call(node(),repo_mgr,all_services,[],5000),
    {{service,"myadd","2.0.0"},
     [{release,["myadd-2.0.0.tar",
		_TarFile]}]}=lists:keyfind({service,"myadd","2.0.0"},1,AllServices),
    ok.

repo_get_service_test_xx()->
    {error,[repo_mgr,_,'doesnt exists ',"myadd","glurk"]}=rpc:call(node(),repo_mgr,get_service,["myadd","glurk"],5000),
    {error,[repo_mgr,_,'doesnt exists ',glurk,"3.0.0"]}=rpc:call(node(),repo_mgr,get_service,[glurk,"3.0.0"],5000),

    {ok,"myadd-3.0.0.tar",_Binary}=rpc:call(node(),repo_mgr,get_service,["myadd","3.0.0"],5000),
    ok.

repo_2_test_xx()->
    Vsns=rpc:call(node(),repo_mgr,get_vsns,["myadd"],5000),
    "3.0.0"=rpc:call(node(),repo_mgr_lib,latest_vsn,[Vsns],5000),
    "3.0.0"=rpc:call(node(),repo_mgr,latest_release,["myadd"],5000),
    ok.

read_info_test_xx()->
    ["myadd-3.0.0.tar",_TarFile]=rpc:call(node(),repo_mgr,read_service,["3.0.0", "myadd"],5000),
    Vsns=rpc:call(node(),repo_mgr,get_vsns,["myadd"],5000),
    "3.0.0"=rpc:call(node(),repo_mgr_lib,latest_vsn,[Vsns],5000),
    "3.0.0"=rpc:call(node(),repo_mgr,latest_release,["myadd"],5000),
    ok.

repo_delete_latest_test_xx()->
    "3.0.0"=rpc:call(node(),repo_mgr,latest_release,["myadd"],5000),

    _=rpc:call(node(),repo_mgr,delete_service,["myadd","3.0.0"],5000),

    "2.0.0"=rpc:call(node(),repo_mgr,latest_release,["myadd"],5000),
    ok.

repo_create_update_service_test_xx()->
    {ok,store}=rpc:call(node(),repo_mgr,create_update_service,["myadd","../dev"],5000),
    "3.0.0"=rpc:call(node(),repo_mgr,latest_release,["myadd"],5000),
    ok.


stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
