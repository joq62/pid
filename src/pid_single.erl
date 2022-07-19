%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Hw server to control specific hw using conbee II and protocol zigbee
%%% Contains all supported devices   
%%% conbee daemon is running in a docker container called "deconz"
%%% 
%%% Created : 
%%% -------------------------------------------------------------------
-module(pid_single).  

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,?MODULE).

%% External exports
-export([
	 set_temp/1,
	 get_temp/0


	]).


-export([
	 appl_start/1,
	 ping/0,
	 start/5,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(Poll_interval,20).      % 20 seconds
-define(PWM_interval,100). % 100 seconds
-define(PWM_offset,50). % 50/50 duty cycle  
-record(state, {
		k3_node,
		pwm_cycle_time,
		duty_cycle_time,

		pwm_pid,
		temp_sensor_id,
		switch_id,
		ki_value,
		setpoint,
		output,
		input,
		last_error,
		previous_time
	
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

start(K3Node,
      TempSensor,
      Switch,
      IntialSetPoint,
      PwmCycleTime)-> gen_server:start_link(?SERVER, 
					    [K3Node,TempSensor,Switch,IntialSetPoint,PwmCycleTime], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================

get_temp()->
    gen_server:call(?SERVER, {get_temp},infinity).    


set_temp(Temp)->
    gen_server:cast(?SERVER, {set_temp,Temp}).    

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
init([K3Node,TempSensor,Switch,IntialSetPoint,PwmCycleTime]) ->
    pong=net_adm:ping(K3Node),    
    
    {ok, #state{
	    k3_node=K3Node,
	    pwm_pid=undefined,
	    pwm_cycle_time=PwmCycleTime,
	    duty_cycle_time=undefined,
	    temp_sensor_id=TempSensor,
	    switch_id=Switch,
	    ki_value=0,
	    setpoint=IntialSetPoint,
	    output=undefined,
	    input=undefined,
	    last_error=0,
	    previous_time=erlang:system_time(second)},
     0
    }.

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

handle_cast(_Msg, State) ->
  %  rpc:cast(node(),log,log,[?Log_ticket("unmatched cast",[Msg])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) -> % Initial start
    timer:sleep(1100), % Ensure elapsed time >1
    NewState=computePID(State),
    PwmCycleTime=State#state.pwm_cycle_time,
    DutyCycleTime=NewState#state.output,
    io:format("PwmCycleTime,DutyCycleTime ~p~n",[{PwmCycleTime,DutyCycleTime,?MODULE,?FUNCTION_NAME,?LINE}]),
    MyPid=self(),
    {ok,PwmPid}=pwm:start(MyPid,PwmCycleTime,DutyCycleTime),
%    spawn(fun()->control_loop_timer(MyPid,State#state.pwm_cycle_time) end),
    io:format("Initial start of the system ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {noreply, NewState#state{pwm_pid=PwmPid}};

handle_info({control_loop_timeout}, State) -> % Initial start
    NewState=computePID(State),
    io:format("Temp, Setpoint, Output ~p~n",[{NewState#state.input,
					      NewState#state.setpoint,
					      NewState#state.output,
					      ?MODULE,?FUNCTION_NAME,?LINE}]),
    
    MyPid=self(),
    spawn(fun()->control_loop_timer(MyPid,State#state.pwm_cycle_time) end),

    {noreply, NewState};

handle_info({_PwmPid,duty_cycle_timeout}, State) -> % Initial start
    io:format("duty_cycle_timeout ~p~n",[{time(),?MODULE,?FUNCTION_NAME,?LINE}]),
    if
	State#state.pwm_cycle_time=/=State#state.output->
	    turn_off(State);
	true->
	   do_nothing
    end, 
    {noreply, State};

handle_info({PwmPid,pwm_timeout}, State) -> % Initial start
    NewState=computePID(State),
    CycleTime=NewState#state.output,
    io:format("pwm_timeout, CycleTime~p~n",[{CycleTime,time(),?MODULE,?FUNCTION_NAME,?LINE}]),
    PwmPid!{self(),set_duty_cycle_time,CycleTime},
    if
	CycleTime >0->
	    turn_on(NewState);
	true->
	    turn_off(NewState)
    end,
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Info}]),
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched info",[Info])]),
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
%% handle_info(timeout, State) -> % Initial start
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% handle_info(timeout, State) -> % Initial start
%% --------------------------------------------------------------------
turn_on(State)->
    K3Node=State#state.k3_node,
    Switch=State#state.switch_id,
    case rpc:call(K3Node,sd,get,[conbee],5000) of
	{badrpc,Reason}->
	    io:format("badrpc ~p~n",[{Reason,?MODULE,?LINE}]);
	[{ConBeeNode,_}]->
	    rpc:call(ConBeeNode,lumi_switch_n0agl1,set,[Switch,"on"],5000)
    end.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% handle_info(timeout, State) -> % Initial start
%% --------------------------------------------------------------------
turn_off(State)->
    K3Node=State#state.k3_node,
    Switch=State#state.switch_id,
     case rpc:call(K3Node,sd,get,[conbee],5000) of
	 {badrpc,Reason}->
	     io:format("badrpc ~p~n",[{Reason,?MODULE,?LINE}]);
	 [{ConBeeNode,_}]->
	     rpc:call(ConBeeNode,lumi_switch_n0agl1,set,[Switch,"off"],5000)
     end.


%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% handle_info(timeout, State) -> % Initial start
%% --------------------------------------------------------------------
read_temp(K3Node,TempId)->
    [{ConBeeNode,_}]=rpc:call(K3Node,sd,get,[conbee],2000),
    T1=rpc:call(ConBeeNode,lumi_weather,temp,[TempId],5000),
    list_to_float(T1).
%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% handle_info(timeout, State) -> % Initial start
%% --------------------------------------------------------------------
control_loop_timer(Pid,Timeout)->
    timer:sleep(Timeout*1000),
    Pid!{control_loop_timeout}.

%% --------------------------------------------------------------------
%% Func: computePID
%% Purpose: Convert process state when code is changed
%% MaxDutyCycle=State#state.pwm_cycle_time,
%% +5 > grader -> MaxDutyCycle
%% -5 < grader -> MinDutyCycle
%%  +5 > grader > -5 => MaxDutyCycle/10, 
%%  Ki -> 1 steg per 10 min 
%%  Kd -> 1 steg per 2 grader
%%  Kp -> 1 steg per diff 
%%
%% --------------------------------------------------------------------
-define(Kp,3).
-define(Ki,1/100).
-define(Kd,40).
-define(MaxDiffPositiv,3).
-define(MaxDiffNegative,-3).


computePID(State)->
    MaxDutyCycle=State#state.pwm_cycle_time,
    Offset=trunc(MaxDutyCycle*0.5),
    MinDutyCycle=0,
    

    CurrentTime=erlang:system_time(second),
    ElapsedTime=CurrentTime-State#state.previous_time,

    Input=trunc(read_temp(State#state.k3_node,State#state.temp_sensor_id)),
    Error=State#state.setpoint-Input,
    RateError=(Error-State#state.last_error)/ElapsedTime,

    if
	Error>?MaxDiffPositiv->
	    CumError=State#state.ki_value,	    
	    RawOutput=MaxDutyCycle,
	    Output=MaxDutyCycle;
	Error<?MaxDiffNegative->
	    CumError=State#state.ki_value,	    
	    RawOutput=MinDutyCycle,
	    Output=MinDutyCycle;
	true->
	    CumError=State#state.ki_value+(Error*ElapsedTime),
	    RawOutput=trunc(?Kp*Error+?Ki*CumError+?Kd*RateError)+Offset,
	    Output=if
		       RawOutput<0->
			   0;
		       RawOutput>MaxDutyCycle->
			   MaxDutyCycle;
		       true->
			   RawOutput
		   end
    end, 
    io:format("******************* ~p **************************~n",[{time(),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("ElapsedTime = ~p~n",[ElapsedTime]),
    io:format("Input = ~p~n",[Input]),
    io:format("Error = ~p~n",[Error]),
    io:format("CumError = ~p~n",[CumError]),
    io:format("RateError = ~p~n",[RateError]),
    io:format("Formel  = ~w, Kp~p+Ki~p+Kd~p + Offset~p  ~n",[Output,?Kp*Error,?Ki*CumError,?Kd*RateError,Offset]),

  %  io:format("Formel Output = ~p, Kp ~p, ~n",[Output,?Kp*Error*Offset]),
    
    io:format("RawOutput = ~p~n",[RawOutput]),
    io:format("Output = ~p~n",[Output]),
    io:format("-------------------------- ~p ----------------------------- ~n",["end "]),
    

    NewState=State#state{input=Input,output=Output,last_error=Error,ki_value=CumError,previous_time=CurrentTime},
    NewState.
