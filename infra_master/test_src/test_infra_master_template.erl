%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_infra_master_template).

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
%% 4. Build applications and app_specs 
%% --------------------------------------------------------------------
build_template_application_test()->
    ServiceDir="template",
    DevAreaDir="../../dist_erlang-1.0.0",
    {ok,store}=repo_mgr:create_update_service(ServiceDir,DevAreaDir),
    {ok,"template-3.0.0.tar",_Binary}=repo_mgr:get_service("template","3.0.0"),
    ok.

build_ppool_test()->
    ServiceDir="ppool",
    DevAreaDir="../../dist_erlang-1.0.0",
    {ok,store}=repo_mgr:create_update_service(ServiceDir,DevAreaDir),
    {ok,"ppool-1.0.0.tar",Binary}=repo_mgr:get_service("ppool","1.0.0"),
    
    ok.

store_template_app_file_test()->
    AppBaseName="template.app_spec",
    AppFileName="app_specs/template.app_spec",
    VsnAppFile="1.23.42" ,
    {ok,Binary}=file:read_file(AppFileName),
    {ok,store}=infra_master:store_appfile(AppBaseName,VsnAppFile,Binary),
    {AppBaseName,Binary}=infra_master:get_appfile(AppBaseName,VsnAppFile),
    ok.


store_ppool_app_file_test()->
    AppBaseName="ppool.app_spec",
    AppFileName="app_specs/ppool.app_spec",
    VsnAppFile="1.0.4" ,
    {ok,Binary}=file:read_file(AppFileName),
    {ok,store}=infra_master:store_appfile(AppBaseName,VsnAppFile,Binary),
    {AppBaseName,Binary}=infra_master:get_appfile(AppBaseName,VsnAppFile),
    ok.


%  [{service,"test"}, {vsn,"1.2.3"},{board_type,all}, {numInstances,100}, {geo,no}]


%% josca test

%%

josca_test()->
    AppBaseName="template.app_spec",
    VsnAppFile="1.23.42",
    "template"=josca:service(AppBaseName,VsnAppFile),
    "3.0.0"=josca:service_vsn(AppBaseName,VsnAppFile),
    []=josca:zone(AppBaseName,VsnAppFile),
    []=josca:board_functionality(AppBaseName,VsnAppFile),
    100=josca:num_instances(AppBaseName,VsnAppFile),
    []=josca:geo_red(AppBaseName,VsnAppFile),
    [{"ppool.app_spec","1.0.4"}]=josca:needed_services(AppBaseName,VsnAppFile),

    ok.


josca_start_order_test()->
    AppBaseName="template.app_spec",
    VsnAppFile="1.23.42",
    [{"ppool","1.0.0",[],[],1,[]},{"template","3.0.0",[],[],100,[]}]=josca:start_order(AppBaseName,VsnAppFile),
    ok.


store_app_sx_test()->
    store_app_file("s1.app_spec","app_specs/s1.app_spec","1.0.1"),
    store_app_file("s2.app_spec","app_specs/s2.app_spec","1.0.2"),
    store_app_file("s3.app_spec","app_specs/s3.app_spec","1.0.3"),
    store_app_file("s4.app_spec","app_specs/s4.app_spec","1.0.4"),
    store_app_file("s5.app_spec","app_specs/s5.app_spec","1.0.5"),
    store_app_file("s6.app_spec","app_specs/s6.app_spec","1.0.6"),
    store_app_file("s7.app_spec","app_specs/s7.app_spec","1.0.7"),
    ok.

josca_start_order_sx_test()->
    AppBaseName="s1.app_spec",
    VsnAppFile="1.0.1",
    [{"s7","1.7.0",[],[],7,[]},
     {"s3","1.3.0",[],[],3,[]},
     {"s6","1.6.0",[zone_1],[],6,[zone_6]},
     {"s5","1.5.0",[],[],5,[]},
     {"s4","1.4.0",[],[],4,[zone_11,zone_12]},
     {"s2","1.2.0",[zone_1],[],2,[zone_1,zone_2]},
     {"s1","1.1.0",[],[],100,[]}]=josca:start_order(AppBaseName,VsnAppFile),
    ok.

store_app_file(AppBaseName,AppFileName,VsnAppFile)->
    {ok,Binary}=file:read_file(AppFileName),
    {ok,store}=infra_master:store_appfile(AppBaseName,VsnAppFile,Binary),
    {AppBaseName,Binary}=infra_master:get_appfile(AppBaseName,VsnAppFile),
    ok.
