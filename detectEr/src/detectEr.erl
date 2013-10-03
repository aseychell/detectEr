%% @author Aldrin
%% @doc @todo Add description to detectEr.
-module(detectEr).

% REFERENCE: MC ERLANG
-define(debug,true).
-include("macros.hrl").
% /REFERENCE: MC ERLANG

%% ====================================================================
%% API functions
%% ====================================================================
-export([m_fls/0, m_nec/4, m_nec_set/2, m_and/2, m_max/2, m_var/1,    
         m_trace/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_fls() ->  
	fun (_Mappings) ->	
        case whereis(?FAIL_MON) of
            undefined -> ok;
            Pid -> Pid ! fail
        end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
m_nec(Type, ActorSpec, MessageSpec, Phi) ->
	fun (Mappings) ->
		 receive
             kill -> ok;
			 {action, {TypeAction, Actor, Message}} ->
				 
                 case TypeAction of
                     Type ->
                         case ActorSpec(Actor) of
        					 true -> 	
                                 case MessageSpec(Message) of
                                     true -> Phi(Mappings);
                                     _ -> ok
                                 end;
                             _ -> ok
        				 end;
                     _ -> ok
                 end
%% 				 case elementOf(Action, [{Type, ActorSpec,MessageSpec}]) of
%% 					 in -> 	Phi(Mappings);
%%                      _ -> ok
%% 				 end
		 end
	end. 

%% Modal = in | notin
%% SetElem = {recv, Receiver, Message} or {send, From, To, Message}
%% Set = [SetElem, SetElem, ...]
m_nec_set({Modal, Set}, Phi) ->
    fun (Mappings) ->
        receive
            kill -> ok;
            {action, Action} ->
                case elementOf(Action,Set) of
                    Modal -> 
                        ?LOG("Proceeding...",[]),
                        Phi(Mappings);
                    _ -> ok
                end
        end
    end.

elementOf(_Action, []) ->
    notin;
elementOf(Action = {Type, Actor, Message}, [{Type, ActorSpec, MessageSpec} | Rest]) ->
    case ActorSpec(Actor) of
         true -> case MessageSpec(Message) of
                    true -> in;
                    _ -> elementOf(Action, Rest)
                 end;
         _ -> elementOf(Action, Rest)
    end;
elementOf(Action, [_A | Rest]) ->
    elementOf(Action, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
m_and(Phi1, Phi2) ->
	fun (Mappings) ->
		 A = spawn(fun () -> Phi1(Mappings) end),
		 B = spawn(fun () -> Phi2(Mappings) end),
		 fork(A, B)
	end.

fork(A, B) ->
	receive
        kill -> 
            A ! kill, B ! kill,
            killed;
		Msg ->
			A ! Msg,
			B ! Msg,
            fork(A, B)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_max(Var, Phi) ->
	fun (Mappings) ->
		Phi([{Var, Phi} | Mappings])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_var(Var) ->
	fun(Mappings) ->
			Monitor = lookUp(Var, Mappings),
			Monitor(Mappings)
	end.

lookUp(Var, Mappings) ->
	case Mappings of
		[{Var, Monitor} | _Other] -> Monitor;
		[ _ | Tail] -> lookUp(Var, Tail);
		[] -> exit
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_trace() ->
	fun(_Mappings) ->
			receivePrintActions() 
    end.

receivePrintActions() ->
	receive
        kill -> ok;
		M ->  M, receivePrintActions()
	end.
