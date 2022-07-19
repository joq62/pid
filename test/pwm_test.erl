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
-module(pwm_test).    
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(Switch,"switch_balcony_heater").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    setup(),
    
    MyPid=self(),
    {ok,P1}=pwm:start(MyPid,4*1000,2*1000),
    {ok,P2}=pwm:start(MyPid,10*1000,1*1000),
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
