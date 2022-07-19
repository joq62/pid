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
-module(basic_eunit).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(K3Node,'solis_node@c202').
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=setup(),
    pid_single_test:start(),
 %   pwm_test:start(),
    
    

 %   pong=net_adm:ping(?K3Node),
  %  [{ConBeeNode,_}]=rpc:call(?K3Node,sd,get,[conbee],2000),
    
  %  rpc:call(ConBeeNode,lumi_switch_n0agl1,set,["switch_balcony_heater","on"],5000),
  %  timer:sleep(1000),
  %  rpc:call(ConBeeNode,lumi_switch_n0agl1,set,["switch_balcony_heater","off"],5000),

  %  t1(ConBeeNode),
%    timer:sleep(5000),
%   init:stop(),
    ok.
	       
t1(ConBeeNode)->
    T1=rpc:call(ConBeeNode,lumi_weather,temp,["temp_indoor_house"],5000),
    io:format(" temp = ~p~n",[{T1,list_to_float(T1)}]),
    timer:sleep(5000),
    t1(ConBeeNode).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
  
    % Simulate host
  %  R=rpc:call(node(),test_nodes,start_nodes,[],2000),
%    [Vm1|_]=test_nodes:get_nodes(),

%    Ebin="ebin",
 %   true=rpc:call(Vm1,code,add_path,[Ebin],5000),
    R=ok,
    R.
