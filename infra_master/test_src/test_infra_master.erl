%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_infra_master).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").




%% --------------------------------------------------------------------
-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------

%% --------------------------------------------------------------------
%% 1. Initial set up
%% --------------------------------------------------------------------
start()->
    % Initialization of the tests 
    io:format("**********   Init system ~n"),
    test_infra_master_start:test(),
    io:format(" ~n"),
      
    % Test the Josca service templates
    io:format(">>>>>   Josca Template test ~n"),
    test_infra_master_template:test(),
    io:format(" ~n"),

    % Load and start a service 
    io:format(">>>>>   Load and start a servicetest ~n"),
    test_infra_master_load_app:test(),
    io:format(" ~n"),

    % Stop and clean up 
    io:format("***********    Clean up and stop test ~n"),
    test_infra_master_stop:test(),
    io:format(" ~n"),
    ok.
   
