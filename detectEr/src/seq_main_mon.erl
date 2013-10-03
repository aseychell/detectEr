%% Author: Aldrin
%% Created: 26 Apr 2012
%% Description: TODO: Add description to main_mon
-module(seq_main_mon).

%%
%% Include files
%%

% REFERENCE: MC ERLANG
-define(debug,true).
-include("macros.hrl").
% /REFERENCE: MC ERLANG

-compile(export_all).

-export([start/2, start_foreach/2,
         start_server/0, stop_server/1,
         monitor_process/3, start_property/2, init_server/0, new_vars/0,
         m_fls/0, m_nec/3, m_and/2, m_max/2, m_var/1, m_trace/0]).

% This assumes fail server is up and running before calling this function.
start(SystemPid, Property) ->
    Monitor = start_server(),
    monitor_process(Monitor, [SystemPid], true),
    start_property(Monitor, Property),
    Monitor.

start_foreach([], _Property) ->
    [];
start_foreach([Pid | Pids], Property) ->
    [start(Pid, Property) | start_foreach(Pids, Property)].

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%     API    %%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_server() ->
    spawn(?MODULE, init_server, []).

stop_server(Monitor) ->
    Monitor ! {stop, user}.

monitor_process(Monitor, Pid, Enable) ->
    Monitor ! {add_tracing, Pid, Enable}.

start_property(Monitor, Property) ->
    Monitor ! {start_monitoring, Property}.

new_vars() ->
    dict:new().



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
        {start_monitoring, Property} ->
            loop_main_matcher([Property], nil)
    end.

trace([], _Enable) ->
    ok;
trace([Pid | Pids], Enable) ->
    erlang:trace(Pid, Enable, [set_on_spawn, send, 'receive']),
    trace(Pids, Enable).


%% The main loop of the monitor
%% This function receives all trace messages from the traced processes and communicates with isolated Monitors
%% To reason about the performed actions of the traced process.
loop_main_matcher(Monitors, nil) ->
    receive
        {stop, Reason} ->
            stop(Reason);
        {trace, Receiver, 'receive', Msg} ->
            NewMons = takeAction(Receiver, Monitors, {recv, Receiver, Msg}),
            loop_main_matcher(NewMons, nil);
        {trace, Sender, send, Msg, To} ->
            loop_main_matcher(Monitors, {send, Sender, To, Msg});
        {test, finished, Pid} ->
            Pid ! {monitor, finished};
        _Msg ->
            loop_main_matcher(Monitors, nil)
    end;
loop_main_matcher(Monitors, Previous={send, _PrevSender, PrevTo, PrevMsg}) ->
    receive
        {stop, Reason} ->
            stop(Reason);
        {trace, PrevTo, 'receive', PrevMsg} ->
            loop_main_matcher(Monitors, nil);
        {trace, Receiver, 'receive', Msg} ->
            NewMons = takeAction(Receiver, Monitors, Previous),
            NewMons2 = takeAction(Receiver, NewMons, {recv, Receiver, Msg}),
            loop_main_matcher(NewMons2, nil);
        {trace, Sender, send, Msg, To} ->
            NewMons = takeAction(PrevTo, Monitors, Previous),
            NewMons2 = takeAction(To, NewMons, {send, Sender, To, Msg}),
            loop_main_matcher(NewMons2, nil);
        {test, finished, Pid} ->
            Pid ! {monitor, finished};
        _MSG ->
            loop_main_matcher(Monitors, nil)
    after 0 ->
            NewMons = takeAction(PrevTo, Monitors, Previous),
            loop_main_matcher(NewMons, nil)
    end.

stop(Reason) ->
    {stop, Reason}.
%%     unregister(?MAIN_MON).

applyAction([], _Action) ->
    [];
applyAction([{_SubProperties, -1, _Vars} | Rest], Action) ->
    applyAction(Rest, Action);
applyAction([{SubProperties, Index, Vars} | Rest], Action) ->
%%     ?LOG("In apply: ~p", [Props]),
    Applied = applyAction1(SubProperties, {Index, Vars}, Action),
    case Applied of 
        {_Properties, -1, _Vars} ->
          applyAction(Rest, Action);
        NewProperty ->
          [NewProperty | applyAction(Rest, Action)]
    end;
applyAction(Props, Action) ->
    ?LOG("NOT MATCHED: ~p || ~p",[Props, Action]),
    fail.

applyAction1(_Props, {-1, _Vars}, _Action) ->
    {[], -1, new_vars()};
applyAction1(Props, {Index, Vars}, Action) ->
    {PrevPredicate, Evaluator, InternalState} = lists:nth(Index, Props),
    
    %% Update the monitor at the Index position and return also the new global state
    case Evaluator(InternalState, {Index,Vars}, Action) of
        {not_applied, {NewInternalState, {NewIndex, NewVars}}} ->
            NewProps = m_utils:setnth(Index, Props, {PrevPredicate, Evaluator, NewInternalState}),
            applyAction1(NewProps, {NewIndex, NewVars}, Action);
        {replace, Phi} ->
            NewProps = m_utils:setnth(Index, Props, Phi),
            applyAction1(NewProps, {Index, Vars}, Action);
        finished ->
            {[], -1, new_vars()};
        {_NewInternalState, {-1, _NewGlobalVars}} ->
            {[], -1, new_vars()};
        {NewInternalState, {NewGlobalIndex, NewGlobalVars}} ->
            {UpdatedMons, NewGlobalIndex, NewGlobalVars} = {m_utils:setnth(Index, Props, {PrevPredicate, Evaluator, NewInternalState}), NewGlobalIndex, NewGlobalVars},
            case lists:nth(NewGlobalIndex, UpdatedMons) of
                {true, _NewEvaluator, _NewIS} -> 
                        % For the case of the false, we do not need another action to evaluate it, therefore we apply it again
                        applyAction1(UpdatedMons, {NewGlobalIndex, NewGlobalVars}, Action);
                _ -> {UpdatedMons, NewGlobalIndex, NewGlobalVars}
            end
    end.
    



%%%%%%%%% MONITORING FUNCTIONS %%%%%%%%%%%%%%%%%

m_trace() ->
    {false, fun (InternalState,GlobalState,Action) ->
%%         ?LOG("TRACE: Action[~p]",[Action]),
        {InternalState,GlobalState}
     end, nil}.     

m_fls() ->
    {true, 
     fun (_, _, _) ->
        case whereis(?FAIL_MON) of
            undefined -> ok;
            Pid -> Pid ! fail
        end,
        stop(fail),
        finished
    end, 
    nil}.
           

m_nec(ActionType1, ActorSpec, MessageSpec) ->
    Evaluator = fun(InternalState, {Index, Vars}, {ActionType2, Actor, Message}) ->
                    case ActionType1 of
                        ActionType2 ->
                            case ActorSpec(Actor) of
                                true -> 
                                    case MessageSpec(Message) of
                                            true -> {InternalState, {Index + 1, Vars}};
                                            _ -> {InternalState, {-1, Vars}}
                                        end;
                                _ -> {InternalState, {-1, Vars}}
                            end;
                        _ -> {InternalState, {-1, Vars}}
                    end
                end,
    {false, Evaluator, nil}.
    
m_and(Property1, Property2) ->
    Evaluator = fun(InternalState, {GlobalIndex, GlobalVars}, Action) ->
                   {{Monitors1, InternalIndex1, InternalVars1},  
                    {Monitors2, InternalIndex2, InternalVars2}} = InternalState,
                   
                   MergeFun = fun(_Key, _Value1, Value2) -> Value2 end,
                   
                   ResultProperty1 = applyAction1(Monitors1, {InternalIndex1, dict:merge(MergeFun, GlobalVars, InternalVars1)}, Action),
                   ResultProperty2 = applyAction1(Monitors2, {InternalIndex2, dict:merge(MergeFun, GlobalVars, InternalVars2)}, Action),
                   {{ResultProperty1, ResultProperty2}, {GlobalIndex, GlobalVars}}
                end,
    InitialState =  {Property1,Property2},
    {false, Evaluator, InitialState}.

m_max(Var, Phi) ->
    {false, fun(InternalState, {Index, Vars}, _Action) ->
            %% Add the var to the vars list with the index + 1 so as not to start from MAX after recursion
            UpdatedVars = dict:store(Var, Phi, Vars),
            {not_applied, {InternalState, {Index+1, UpdatedVars}}} 
    end, nil}.

m_var(Var) ->
    {false, fun(_InternalState, {_Index, Vars}, _Action) ->
        %% lookup var in Vars and return the referred Index
        {ok, Phi} = dict:find(Var, Vars),
        {replace, Phi}
     end, nil}.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
takeAction(_OtherPid, Monitors, Action = {recv, _RPid, Msg}) ->
    case Msg of
        timeout -> Monitors;
        {io_reply, _, _} -> Monitors;
        {file_reply, _, _} -> Monitors;
        {code_server, _} -> Monitors;
        {X, _} ->
            case m_utils:isSystemProcess(X) of
                true -> Monitors;
                false -> applyAction(Monitors, Action)
            end;
        _ -> applyAction(Monitors, Action)
    end;
takeAction(_OtherPid, Monitors, {send, Sender, To, Msg}) ->
    case m_utils:isSystemProcess(Sender) of
        false -> case m_utils:isSystemProcess(To) of
                   false -> case Msg of
                              {io_request, _, _, _} -> Monitors;
                              _ -> applyAction(Monitors, {send, To, Msg})
                            end;
                   _ -> Monitors
                 end;
        _ -> ok
    end;
takeAction(OtherPid, Monitors, Action) ->
    case m_utils:isSystemProcess(OtherPid) of
        false -> applyAction(Monitors, Action);
        _ -> Monitors
    end.