%% Author: Aldrin
%% Created: 26 Apr 2012
%% Description: TODO: Add description to main_mon
-module(conc_main_mon).

%%
%% Include files
%%

% REFERENCE: MC ERLANG
-define(debug,true).
-include("macros.hrl").
% /REFERENCE: MC ERLANG

%%
%% Exported Functions
%%
-export([start/3, start_foreach/3, 
         trace/2,
         traceProperty/0,
         start_server/0, stop_server/1, monitor_process/3, start_property/3, init_server/0]).

% This assumes fail server is up and running before calling this function.
start(SystemPid, Property, PropertyArgs) ->
    Monitor = start_server(),
    monitor_process(Monitor, [SystemPid], true),
    start_property(Monitor, Property, PropertyArgs),
    Monitor.

start_foreach([], _Property, _PropertyArgs) ->
    [];
start_foreach([Pid | Pids], Property,PropertyArgs) ->
    [start(Pid, Property,PropertyArgs) | start_foreach(Pids, Property, PropertyArgs)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%     API    %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_server() ->
	spawn(?MODULE, init_server, []).


stop_server(Monitor) ->
    Monitor ! {stop, self(), user}.

monitor_process(Monitor, Pid, Enable) ->
	Monitor ! {add_tracing, Pid, Enable}.

start_property(Monitor, Property, PropertyArgs) ->
	Monitor ! {start_monitoring, Property, PropertyArgs}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%  INTERNAL  %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_server() ->
	loop_init().

loop_init() ->
	receive
		{add_tracing, Pid, Enable} ->
			trace(Pid, Enable),
			loop_init();
		{start_monitoring, Property, PropertyArgs} ->
			?LOG("Starting monitoring for property ~p with args ~p", [Property, PropertyArgs]),
			InitialisedProperty = Property(PropertyArgs),
			TopMonitor = m_utils:startMonitor(InitialisedProperty, []),
			loop_main_matcher(TopMonitor, nil)
	end.
	

trace([], _Enable) ->
    ok;
trace([Pid | Pids], Enable) ->
    erlang:trace(Pid, Enable, [set_on_spawn, send, 'receive']),
    trace(Pids, Enable).


%% The main loop of the monitor
%% This function receives all trace messages from the traced processes and communicates with isolated Monitors
%% To reason about the performed actions of the traced process.
loop_main_matcher(Monitor, nil) ->
    receive
        {stop, From, Reason} ->
            Monitor ! kill,
            From ! stop(Reason);
		{trace, Receiver, 'receive', Msg} ->
			takeAction(Receiver, Monitor, {recv, Receiver, Msg}),
			loop_main_matcher(Monitor, nil);
		{trace, Sender, send, Msg, To} ->
			loop_main_matcher(Monitor, {send, Sender, To, Msg});
        {test, finished, Pid} ->
            Monitor ! kill,
            Pid ! {monitor, finished};
		_Msg ->
			loop_main_matcher(Monitor, nil)
    end;
loop_main_matcher(Monitor, Previous={send, _PrevSender, PrevTo, PrevMsg}) ->
    receive
        {stop, From, Reason} ->
            Monitor ! kill,
            From ! stop(Reason);
		{trace, PrevTo, 'receive', PrevMsg} ->
			loop_main_matcher(Monitor, nil);
		{trace, Receiver, 'receive', Msg} ->
			% No matching with buffer .. process both
			takeAction(Receiver, Monitor, Previous),
			takeAction(Receiver, Monitor, {recv, Receiver, Msg}),
			loop_main_matcher(Monitor, nil);
		{trace, Sender, send, Msg, To} ->    
			% No matching with buffer .. process both
			takeAction(PrevTo, Monitor, Previous),
			takeAction(To, Monitor, {send, Sender, To, Msg}),
			loop_main_matcher(Monitor, nil);
        {test, finished, Pid} ->
            Monitor ! kill,
            Pid ! {monitor, finished};
		_MSG ->
			loop_main_matcher(Monitor, nil)
	after 0 ->
			takeAction(PrevTo, Monitor, Previous),
			loop_main_matcher(Monitor, nil)
    end.


stop(Reason) ->
    ?LOG("Stopping ... ~p",[Reason]).


%% Take action functions are needed in order to filter system (Erlang VM) msgs and system events
takeAction(_OtherPid, _Monitor, {recv, _RPid, mon_start}) ->
	ok;
takeAction(_OtherPid, Monitor, Action = {recv, _RPid, Msg}) ->
    case Msg of
        timeout -> ok;
        {io_reply, _, _} -> ok;
        {file_reply, _, _} -> ok;
        {code_server, _} -> ok;
        {X, _} ->
            case m_utils:isSystemProcess(X) of
                true -> ok;
                false -> Monitor ! {action, Action}
            end;
        _ -> Monitor ! {action, Action}
    end;
takeAction(_OtherPid, Monitor, {send, Sender, To, Msg}) ->
    case m_utils:isSystemProcess(Sender) of
        false -> case m_utils:isSystemProcess(To) of
				   false -> case Msg of
				                {io_request, _, _, _} -> ok;
				                _ -> Monitor ! {action, {send, To, Msg}}
				            end;
				   _ -> ok
			     end;
        _ -> ok
    end;
takeAction(OtherPid, Monitor, Action) ->
    case m_utils:isSystemProcess(OtherPid) of
        false -> Monitor ! {action, Action};
        _ -> ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%     DEBUG   %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
traceProperty() ->
	fun(_Procs) ->
			detectEr:m_trace()	
    end.

