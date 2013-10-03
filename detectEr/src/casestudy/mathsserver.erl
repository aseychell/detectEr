%% Author: Aldrin
-module(mathsserver).

%%
%% Include files
%%
-define(debug,false).
-include("macros.hrl").
%%
%% Exported Functions
%%
-export([start/0, stop/1, 
         factorial/2, sumlist/2, summation/2, 
         init/3, loop/3, 
         factorial_worker/0, sumlist_worker/0, summation_worker/0]).

-define(FAIL_FLAG, true).
-define(WORKERS_COUNT, 8).
-define(SLOW_DOWN_FACTOR, 70).

%%
%% API Functions
%%
start() ->
    FactorialWorkers = m_utils:callNTimes(?WORKERS_COUNT, fun() -> spawn(?MODULE, factorial_worker, []) end),
    SumWorkers = m_utils:callNTimes(?WORKERS_COUNT, fun() -> spawn(?MODULE, summation_worker, []) end),
    SumlistWorkers = m_utils:callNTimes(?WORKERS_COUNT, fun() -> spawn(?MODULE, sumlist_worker, []) end),
    
    Server = spawn(?MODULE, init, [FactorialWorkers, SumWorkers, SumlistWorkers]),
    {Server, FactorialWorkers ++ SumWorkers ++ SumlistWorkers}.

stop(Server) ->
	Server ! {stop, normal}.
 
factorial(Server, Value) ->
    Server ! {request, factorial, self(), Value},
    receive
        {result, Result} -> Result
    end.

sumlist(Server, List) ->
    Server ! {request, sumlist, self(), List},
    receive
        {result, Result} -> Result
    end.
summation(Server, Value) ->
    Server ! {request, summation, self(), Value},
    receive
        {result, Result} -> Result
    end.


%%%%%%%%%%%%%% Server %%%%%%%%%%%%%%%%%%%%%

init(FactorialWorkers, SumWorkers, SumlistWorkers) ->
    loop(FactorialWorkers, SumWorkers, SumlistWorkers).

loop(FactWorkers = [FactWorker | RestFactWorkers], 
     SumWorkers = [SumWorker | RestSumWorkers], 
     SumlistWorkers = [SumlistWorker | RestSumlistWorkers]) ->
    
	receive
        {query_workers, pids, From} ->
            From ! {query_workers, result, FactWorkers ++ SumWorkers ++ SumlistWorkers},
            loop(FactWorkers, SumWorkers, SumlistWorkers);
		{stop, _Reason} ->
            Workers = FactWorkers ++ SumWorkers ++ SumlistWorkers,
            lists:foreach(fun(Worker) -> Worker ! kill end, Workers),
			stop;
		{request, factorial, PidFrom, RequestValue} ->
            FactWorker  ! {request, RequestValue, self(), PidFrom},
            loop(RestFactWorkers ++ [FactWorker], SumWorkers, SumlistWorkers);
		{request, summation, PidFrom, RequestValue} ->
            SumWorker  ! {request, RequestValue, self(), PidFrom},
            loop(FactWorkers, RestSumWorkers ++ [SumWorker], SumlistWorkers);
		{request, sumlist, PidFrom, RequestValue} ->
            SumlistWorker  ! {request, RequestValue, self(), PidFrom},
            loop(FactWorkers, SumWorkers, RestSumlistWorkers ++ [SumlistWorker]);
        {result, Result, OriginalClient} ->
            OriginalClient ! {result, Result},
            send_duplicate(OriginalClient, {result,Result}),
            loop(FactWorkers, SumWorkers, SumlistWorkers)
	end.

send_duplicate(To, Msg) ->
    case ?FAIL_FLAG of
        true ->
            case get(count) of
                5 -> To ! Msg, put(count, 0);
                undefined -> put(count, 0);
                N -> put(count, N+1)
            end;
        _ -> ok
    end.
    
send_duplicate_random(To, Msg) ->
    case ?FAIL_FLAG of
        true ->
            random:seed(erlang:now()),
            Num = random:uniform(90000),
            case Num rem 2 of
                0 -> 
                    random:seed(erlang:now()),
                    Num2 = random:uniform(90000),
                    case Num2 rem 2 of
                        0 -> To ! Msg;
                        _ -> ok
                    end;
                _ -> ok
            end;
        _ -> ok
    end.

factorial_worker() ->
    receive
        kill -> ok;
        {request, RequestValue, Parent, OriginalClient} ->
            m_utils:callNTimes(?SLOW_DOWN_FACTOR, fun() -> factorial(RequestValue) end),
            Result = factorial(RequestValue),
            Parent ! {result, Result, OriginalClient},
            send_duplicate_random(Parent, {result, Result, OriginalClient}),
            factorial_worker()
    end.

factorial(0) -> 1;
factorial(N) -> N * factorial (N-1).

summation_worker() ->
    receive
        kill -> ok;
        {request, RequestValue, Parent, OriginalClient} ->
            m_utils:callNTimes(?SLOW_DOWN_FACTOR, fun() -> summation(RequestValue) end),
            Result = summation(RequestValue),
            Parent ! {result, Result, OriginalClient},
            send_duplicate_random(Parent, {result, Result, OriginalClient}),
            summation_worker()
    end.

summation(0) -> 0;
summation(N) -> N + summation(N-1).

sumlist_worker() ->
    receive
        kill -> ok;
        {request, RequestValue, Parent, OriginalClient} ->
            m_utils:callNTimes(?SLOW_DOWN_FACTOR, fun() -> sumlist(RequestValue) end),
            Result = sumlist(RequestValue),
            Parent ! {result, Result, OriginalClient},
            send_duplicate_random(Parent, {result, Result, OriginalClient}),
            sumlist_worker()
    end.

sumlist([]) -> 0;
sumlist([X | Rest]) -> X + sumlist(Rest).