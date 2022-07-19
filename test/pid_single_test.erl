%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(pid_single_test).    
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(Switch,"switch_balcony_heater").
-define(TempSensor,"temp_indoor_house").
-define(K3Node,'solis_node@c202').
-define(IntialSetPoint,22).
-define(PwmCycleTime,20).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    setup(),
    
    {ok,P1}=pid_single:start(?K3Node,?TempSensor,?Switch,?IntialSetPoint,?PwmCycleTime),
  
    loop(),
 %   spawn(fun()->loop(P2) end),
%    init:stop(),
    ok.

	     
 loop()->
    receive
	{Pid,Msg}->
	    io:format("Msg from Pid ~p~n",[{Pid,Msg}])
    end,
    loop().

 loop(Pid)->
    receive
	{Pid,Msg}->
	    io:format("Msg from Pid ~p~n",[{Pid,Msg}])
    end,
    loop(Pid).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------


setup()->

   ok.
