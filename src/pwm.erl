%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Hw server to control specific hw using conbee II and protocol zigbee
%%% Contains all supported devices   
%%% conbee daemon is running in a docker container called "deconz"
%%% 
%%% Created : 
%%% -------------------------------------------------------------------
-module(pwm).  

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,?MODULE).

%% External exports
-export([
	 set_duty_cycle_time/1,
	 get_duty_cycle_time/0,
	 set_pwm_cycle_time/1,
	 get_pwm_cycle_time/0,


	 duty_cycle_timeout/0,
	 pwm_timeout/0

	]).


-export([

	 appl_start/1,
	 ping/0,
	 start/3,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PwmCycleTime,5). % 100 seconds
-define(DutyCycleTimeoffset,2). % 50/50 duty cycle  
-record(state, {
		parent_pid=undefined,
		pwm_cycle_time=undefined,
		duty_cycle_time=undefined
	       }).

%% ====================================================================
%% External functions
%% ====================================================================
appl_start([])->
    application:start(?SERVER).

%% ====================================================================
%% Server functions
%% ====================================================================
%% Gen server functions

%start(SwitchId)-> gen_server:start_link({local, ?SERVER}, ?SERVER, [SwitchId], []).
start(ParentPid,PwmCycleTime,DutyCycleTime)-> gen_server:start_link(?SERVER, [ParentPid,PwmCycleTime,DutyCycleTime], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================

set_duty_cycle_time(CycleTime)->
    gen_server:cast(?SERVER, {set_duty_cycle_time,CycleTime}).       
get_duty_cycle_time()->
    gen_server:call(?SERVER, {get_duty_cycle_time},infinity).    

set_pwm_cycle_time(CycleTime)->
    gen_server:cast(?SERVER, {set_pwm_cycle_time,CycleTime}).       
get_pwm_cycle_time()->
    gen_server:call(?SERVER, {get_pwm_cycle_time},infinity).   

duty_cycle_timeout()->
    gen_server:cast(?SERVER, {duty_cycle_timeout}).    
pwm_timeout()->
    gen_server:cast(?SERVER, {pwm_timeout}).  
%% ====================================================================
%% Support functions
%% ====================================================================

%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

%% ====================================================================
%% Gen Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([ParentPid,PwmCycleTime,DutyCycleTime]) ->
    
    
    {ok, #state{parent_pid=ParentPid,
	       	pwm_cycle_time=PwmCycleTime,
		duty_cycle_time=DutyCycleTime},
     0}.

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

handle_call({turn_on,"lamps"},_From,State) ->
    Reply=lamps:switch_on("switch_lamps"),
    {reply, Reply, State};
handle_call({turn_off,"lamps"},_From,State) ->
    Reply=lamps:switch_off("switch_lamps"),
    {reply, Reply, State};

handle_call({temp,"indoor"},_From,State) ->
    [{ConbeeNode,_}]=sd:get(conbee),
    Reply=rpc:call(ConbeeNode,lumi_weather,temp,["temp_indoor_house"],5000),
    {reply, Reply, State};


handle_call({status,lamps},_From,State) ->
    Reply=lamps:status(),
    {reply, Reply, State};


handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stopped},_From, State) ->
    Reply=ok,
    {reply, Reply, State};


handle_call({not_implemented},_From, State) ->
    Reply=not_implemented,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched call",[Request, From])]),
    Reply = {ticket,"unmatched call",Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast({pwm_cycle_timeout}, State) ->
    State#state.parent_pid!{self(),pwm_cycle_timeout},
    io:format("duty_cycle_timeout ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {noreply, State};

handle_cast({duty_cycle_timeout}, State) ->
    State#state.parent_pid!{self(),duty_cycle_timeout},
    io:format("duty_cycle_timeout ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched signal ~p~n",[{Msg,?MODULE,?FUNCTION_NAME,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) -> % Initial start
    PwmCycleTime=State#state.pwm_cycle_time,
    DutyCycleTime=State#state.duty_cycle_time,
    MyPid=self(),
%    spawn(fun()->pwm_timer(PwmCycleTime,MyPid) end),
%    spawn(fun()->duty_timer(DutyCycleTime,MyPid) end),    
    spawn(fun()->pwm_duty_timer(PwmCycleTime,DutyCycleTime,MyPid) end),    
    io:format("Initial start of the system ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {noreply, State};

handle_info({pwm_timeout}, State) -> % Initial start
    MyPid=self(),
    State#state.parent_pid!{MyPid,pwm_timeout},
    PwmCycleTime=State#state.pwm_cycle_time,
    DutyCycleTime=State#state.duty_cycle_time,
    io:format("PwmCycleTime,DutyCycleTime ~p~n",[{PwmCycleTime,DutyCycleTime,?MODULE,?FUNCTION_NAME,?LINE}]),

 %    spawn(fun()->pwm_timer(PwmCycleTime,MyPid) end),
%    spawn(fun()->duty_timer(DutyCycleTime,MyPid) end),    
    spawn(fun()->pwm_duty_timer(PwmCycleTime,DutyCycleTime,MyPid) end),  
  %  io:format("Initial start of the system ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {noreply, State};

handle_info({_Parent,set_duty_cycle_time,CycleTime}, State) -> 
    io:format("set_duty_cycle_time ~p~n",[{CycleTime,?MODULE,?FUNCTION_NAME,?LINE}]),
    {noreply, State#state{duty_cycle_time=CycleTime}};


handle_info({duty_cycle_timeout}, State) -> % Initial start
    MyPid=self(),
    State#state.parent_pid!{MyPid,duty_cycle_timeout},

  %  io:format("Initial start of the system ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched signal ~p~n",[{Info,?MODULE,?FUNCTION_NAME,?LINE}]),
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
pwm_duty_timer(PwmCycleTime,DutyCycleTime,MyPid)->
    Diff=PwmCycleTime-DutyCycleTime,
    timer:sleep(DutyCycleTime*1000),   
    MyPid!{duty_cycle_timeout},
    timer:sleep(Diff*1000),   
    MyPid!{pwm_timeout}.


pwm_timer(PwmCycleTime,MyPid)->
    timer:sleep(PwmCycleTime*1000),
 %    io:format("pwm_timeout process ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    MyPid!{pwm_timeout}.


duty_timer(DutyCycleTime,MyPid)->
    timer:sleep(DutyCycleTime*1000),
  %  io:format("pwm_timeout process ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    MyPid!{duty_cycle_timeout}.
