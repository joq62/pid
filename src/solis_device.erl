%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Hw server to control specific hw using conbee II and protocol zigbee
%%% Contains all supported devices   
%%% conbee daemon is running in a docker container called "deconz"
%%% 
%%% Created : 
%%% -------------------------------------------------------------------
-module(solis_device).  
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(DeviceInfo,[{"lamp_hall_strindberg","lampa on green cupboard at hall entrance"},
		  {"lamp_livingroom_floor","Floor lamp by the sofa livingroom"},
		  {"lamp_livingroom_small_board","Lamp on the table in livingroom "}, 
		  {"outlet_switch_tv","Outlet for the TV set"},
		  {"switch_lamps","On/off switch for all lamps"},
		  {"switch_tv","On/Off switch TV set"},
		  {"motion_kitchen","Motion detector near kitchen"},
		  {"motion_entrance","Motion detector outdoor entrance door"},
		  {"temp_indoor_house","Tempsensor inside house"}]).
%% External exports
-export([
	 all_info/0
	 
	]).




%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
all_info()->
    ?DeviceInfo.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

		  
