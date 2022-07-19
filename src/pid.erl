%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Hw server to control specific hw using conbee II and protocol zigbee
%%% Contains all supported devices   
%%% conbee daemon is running in a docker container called "deconz"
%%% 
%%% Created : 
%%% -------------------------------------------------------------------
-module(pid).  

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,?MODULE).

%% External exports
-export([
	 turn_on/1,
	 turn_off/1,
	 status/1,
	 temp/1,
	 appl_start/1,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	
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

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================
status(Device)->
    gen_server:call(?SERVER, {status,Device},infinity).    


turn_on(Device)->
    gen_server:call(?SERVER, {turn_on,Device},infinity).    
turn_off(Device)->
    gen_server:call(?SERVER, {turn_off,Device},infinity).    

%% ====================================================================
%% Support functions
%% ====================================================================
temp(Where)->
    gen_server:call(?SERVER, {temp,Where},infinity).


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
init([]) ->
    control_loop:start(),
    
    {ok, #state{}
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
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

		  
