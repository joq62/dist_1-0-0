%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(josca).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------

%% External exports
-export([service/2,
	 service_vsn/2,
	 zone/2,
	 board_functionality/2,
	 num_instances/2,
	 geo_red/2,
	 needed_services/2,
	 start_order/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions

%% --------------------------------------------------------------------
%% @spec:  service(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
   
%% --------------------------------------------------------------------
%% @spec: zone(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
board_functionality(AppBaseName,VsnAppFile)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {board_functionality,BrdFunc}=lists:keyfind(board_functionality,1,MainServiceInfo),
    file:delete(TempFileName),
    BrdFunc.
    
%% --------------------------------------------------------------------
%% @spec: zone(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
zone(AppBaseName,VsnAppFile)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {zone,Zone}=lists:keyfind(zone,1,MainServiceInfo),
    file:delete(TempFileName),
    Zone.
    


%% --------------------------------------------------------------------
%% @spec:  service(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
service(AppBaseName,VsnAppFile)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {service,ServiceStr}=lists:keyfind(service,1,MainServiceInfo),
    file:delete(TempFileName),
    ServiceStr.

    
    
%% --------------------------------------------------------------------
%% @spec:  service_vsn(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
service_vsn(AppBaseName,VsnAppFile)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {vsn,VsnStr}=lists:keyfind(vsn,1,MainServiceInfo),
    file:delete(TempFileName),
    VsnStr.

%% --------------------------------------------------------------------
%% @spec: num_instances(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
num_instances(AppBaseName,VsnAppFile)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {numInstances,NumInstances}=lists:keyfind(numInstances,1,MainServiceInfo),
    file:delete(TempFileName),
    NumInstances.

%% --------------------------------------------------------------------
%% @spec: geo(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
geo_red(AppBaseName,VsnAppFile)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {geo_red,GeoRed}=lists:keyfind(geo_red,1,MainServiceInfo),
    file:delete(TempFileName),
    GeoRed.


%% --------------------------------------------------------------------
%% @spec:  needed_services(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------
needed_services(AppBaseName,VsnAppFile)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),
    {needed_services,NeededServiceAppSpecs}=lists:keyfind(needed_services,1,AppInfo),
    file:delete(TempFileName),
    NeededServiceAppSpecs.

%% --------------------------------------------------------------------
%% @spec:  needed_services(AppBaseName,VsnAppFile)
%% @spec:  
%% Returns: non
%% --------------------------------------------------------------------

start_order(AppBaseName,VsnAppFile)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),    
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {service,ServiceStr}=lists:keyfind(service,1,MainServiceInfo),
    {vsn,VsnStr}=lists:keyfind(vsn,1,MainServiceInfo),
    {zone,Zone}=lists:keyfind(zone,1,MainServiceInfo),
    {board_functionality,BoardFunc}=lists:keyfind(board_functionality,1,MainServiceInfo),
    {numInstances,NumInstances}=lists:keyfind(numInstances,1,MainServiceInfo),
    {geo_red,GeoRed}=lists:keyfind(geo_red,1,MainServiceInfo),
    {needed_services,NeededServiceAppSpecs}=lists:keyfind(needed_services,1,AppInfo),
    file:delete(TempFileName),
    Acc=[{ServiceStr,VsnStr,Zone,BoardFunc,NumInstances,GeoRed}],
    Result=dfs(NeededServiceAppSpecs,Acc),   
    Result.


dfs([],Acc)->
    Acc;
dfs([{AppBaseName,VsnAppFile}|T],Acc)->
    {AppBaseName,Binary}=infra_master_lib:get_appfile(AppBaseName,VsnAppFile,infra_master),
    TempFileName="temp"++integer_to_list(erlang:system_time()),
    ok=file:write_file(TempFileName,Binary),
    {ok,AppInfo}=file:consult(TempFileName),    
    {main_service,MainServiceInfo}=lists:keyfind(main_service,1,AppInfo),
    {service,ServiceStr}=lists:keyfind(service,1,MainServiceInfo),
    {vsn,VsnStr}=lists:keyfind(vsn,1,MainServiceInfo),
    {zone,Zone}=lists:keyfind(zone,1,MainServiceInfo),
    {board_functionality,BoardFunc}=lists:keyfind(board_functionality,1,MainServiceInfo),
    {numInstances,NumInstances}=lists:keyfind(numInstances,1,MainServiceInfo),
    {geo_red,GeoRed}=lists:keyfind(geo_red,1,MainServiceInfo),
    {needed_services,NeededServiceAppSpecs}=lists:keyfind(needed_services,1,AppInfo),
    file:delete(TempFileName),
    Acc1=[{ServiceStr,VsnStr,Zone,BoardFunc,NumInstances,GeoRed}|Acc],
    Acc2=dfs(NeededServiceAppSpecs,Acc1),
    dfs(T,Acc2).  
