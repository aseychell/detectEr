%% @author Aldrin
%% @doc Fail server is used to receive violation detection messages and handle property violation

-module(fail_server).


% REFERENCE: MC ERLANG
-define(debug,true).
-include("macros.hrl").
% /REFERENCE: MC ERLANG

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_fail/1, start_fail_server/1, stop_fail_server/0]).


start_fail_server(CallbackFun) ->
    spawn(?MODULE, init_fail, [CallbackFun]).

stop_fail_server() ->
    case whereis(?FAIL_MON) of 
        undefined -> ok;
        Pid -> Pid ! stop
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================


init_fail(CallbackFun) ->
    register(?FAIL_MON, self()),
    receive 
        fail ->
            ?LOG("~n~n~n FAILURE!!! ~n~n~n", []),
            stop(fail),
            CallbackFun();
        stop ->
            stop(user),
            CallbackFun()
    end.

stop(fail) ->
    ?LOG("Stopping fail... ~p",[fail]),
    unregister(?FAIL_MON);
stop(Reason) ->
    ?LOG("Stopping fail ... ~p",[Reason]),
    unregister(?FAIL_MON).