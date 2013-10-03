%% @author Aldrin
%% @doc @todo Add description to mths_scenarios.


-module(maths_scenarios).

% REFERENCE: MC ERLANG
-define(debug,true).
-include("macros.hrl").
% /REFERENCE: MC ERLANG

-define(TIMER_BEFORE, 100).
-define(TIMER_AFTER_FINISH, 100).

-define(TIMES_TO_PROFILE, 150).
-define(TIMES_TO_EXERCISE, 1).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test1/1, test2/1, test3/1,
         factorial/3, summation/3]).


measure({M,F,A}, {Number, Tag}) ->
    ?LOG("Starting profiling factorial of ~p for ~p times (~p).", [Number, ?TIMES_TO_PROFILE, Tag]),
    Results = m_utils:callNTimes(?TIMES_TO_PROFILE, {M,F,A}),
    ?LOG("Res: ~w", [Results]),
    ?LOG("Timings (~p): ~w ~n~n(avg. ~.4f)", [Tag, Results, m_utils:average(Results)]),
    Results.

test1(all) ->
    NomonRes =  test1(nomon),
    SeqRes =    test1(seq),
    ConcRes =   test1(conc),
    SeqForRes = test1(seq_forest),
    ConcForRes = test1(conc_forest),
    Avgs = lists:map(fun(Results) -> m_utils:average(Results) end, [NomonRes, SeqRes, ConcRes, SeqForRes, ConcForRes] ),
    ?LOG("~n nomon (~w) ~n seq (~w)~n conc (~w) ~n seq_for (~w) ~n conc_for (~w)", Avgs);

test1(T = nomon) ->
    Number = test1Number(),
    measure({m_utils, profile, [?MODULE, factorial, [nomon, Number, nil]]}, 
            {Number, T});
test1(T = seq) ->
    Number = test1Number(),
    measure({m_utils, profile, [?MODULE, factorial, [seq, Number, maths_properties:prop_alwayscorrect_trace(seq, Number)]]}, 
            {Number, T});
test1(T = seq_forest) ->
    Number = test1Number(),
    measure({m_utils, profile, [?MODULE, factorial, [seq_forest, Number, maths_properties_workers:prop_always_neverdup_resp(seq)]]}, 
            {Number, T});
test1(T = conc) ->
    Number = test1Number(),
    measure({m_utils, profile, [?MODULE, factorial, [conc, Number, maths_properties:prop_alwayscorrect_trace(conc, Number)]]}, 
            {Number, T});
test1(T = conc_forest) ->
    Number = test1Number(),
    measure({m_utils, profile, [?MODULE, factorial, [conc_forest, Number, maths_properties_workers:prop_always_neverdup_resp(conc)]]}, 
            {Number, T}).
test1Number() ->
    100.

test2(T = nomon) ->
    Number = test2Number(),
    
    ?LOG("Starting profiling factorial of ~p for ~p times (~p).", [Number, ?TIMES_TO_PROFILE, T]),
    NoMonResults = m_utils:callNTimes(?TIMES_TO_PROFILE, {m_utils, profile, [?MODULE, factorial, [nomon, Number, nil]]}),
    
    ?LOG("Timings (no monitoring): ~w ~n(avg. ~.4f)", [NoMonResults, m_utils:average(NoMonResults)]);
test2(T = seq) ->
    Number = test2Number(),
    
    ?LOG("Starting profiling factorial of ~p for ~p times (~p).", [Number, ?TIMES_TO_PROFILE, T]),
    SeqResults = m_utils:callNTimes(?TIMES_TO_PROFILE, {m_utils, profile, [?MODULE, factorial, [seq, Number, 
                                                                                       maths_properties:prop_all_alwayscorrect_trace(seq, Number)]]}),
    ?LOG("Timings (sequential): ~w (avg. ~.4f)", [SeqResults, m_utils:average(SeqResults)]);
test2(T = conc) ->
    Number = test2Number(),
    
    ?LOG("Starting profiling factorial of ~p for ~p times (~p).", [Number, ?TIMES_TO_PROFILE, T]),
    ConcResults = m_utils:callNTimes(?TIMES_TO_PROFILE, {m_utils, profile, [?MODULE, factorial, [conc, Number, 
                                                                                         maths_properties:prop_all_alwayscorrect_trace(conc, Number)]]}),
    
    ?LOG("Timings (concurrent): ~w (avg. ~.4f)", [ConcResults, m_utils:average(ConcResults)]).
test2Number() ->
    500.

test3(T = nomon) ->
    Number = test3Number(),
    
    ?LOG("Starting profiling factorial of ~p for ~p times (~p).", [Number, ?TIMES_TO_PROFILE, T]),
    NoMonResults = m_utils:callNTimes(?TIMES_TO_PROFILE, {m_utils, profile, [?MODULE, summation, [nomon, Number, nil]]}),
    
    ?LOG("Timings (no monitoring): ~w (avg. ~.4f)", [NoMonResults, m_utils:average(NoMonResults)]);
test3(T = seq) ->
    Number = test3Number(),
    
    ?LOG("Starting profiling factorial of ~p for ~p times (~p).", [Number, ?TIMES_TO_PROFILE, T]),
    SeqResults = m_utils:callNTimes(?TIMES_TO_PROFILE, {m_utils, profile, [?MODULE, summation, [seq, Number, 
                                                                                       maths_properties:prop_all_alwayscorrect_trace(seq, Number)]]}),
    ?LOG("Timings (sequential): ~w (avg. ~.4f)", [SeqResults, m_utils:average(SeqResults)]);
test3(T = conc) ->
    Number = test3Number(),
    
    ?LOG("Starting profiling factorial of ~p for ~p times (~p).", [Number, ?TIMES_TO_PROFILE, T]),
    ConcResults = m_utils:callNTimes(?TIMES_TO_PROFILE, {m_utils, profile, [?MODULE, summation, [conc, Number, 
                                                                                         maths_properties:prop_all_alwayscorrect_trace(conc, Number)]]}),
    
    ?LOG("Timings (concurrent): ~w (avg. ~.4f)", [ConcResults, m_utils:average(ConcResults)]).
test3Number() ->
    500.
    



exercise(factorial, Server, Number) ->
    timer:sleep(?TIMER_BEFORE),
    Times = ?TIMES_TO_EXERCISE,
    Parent = self(),
    spawn(fun() -> m_utils:callNTimes(Times, {mathsserver, factorial, [Server,Number]}), Parent!ready end ),
%%     spawn(fun() -> m_utils:callNTimes(Times, {mathsserver, factorial, [Server,Number+2]}), Parent!ready end ),
    receive
%%         ready -> receive ready -> timer:sleep(?TIMER_AFTER_FINISH) end
        ready -> ready
    end;
exercise(summation, Server, Number) ->
    timer:sleep(?TIMER_BEFORE),
    Parent = self(),
    spawn(fun() -> m_utils:callNTimes(?TIMES_TO_EXERCISE, {mathsserver, summation, [Server,Number]}), Parent!ready end ),
%%     spawn(fun() -> m_utils:callNTimes(25, {mathsserver, factorial, [Server,Number+2]}), Parent!ready end ),
    receive
        ready -> timer:sleep(?TIMER_AFTER_FINISH)
    end.
    

fail_callback() ->
    Self = self(),
    fun() -> Self ! fail_stopped end.


wait_fail_stopped(0) ->
    fail_server:stop_fail_server(), 
    receive fail_stopped -> monitor_finished end;
wait_fail_stopped(Count) ->
    receive
        fail_stopped ->
            fail_stopped;
        {monitor, finished} ->
            wait_fail_stopped(Count - 1)
    end.

notify_exercise_finished(Monitor) ->
    Monitor ! {test, finished, self()}.


factorial(nomon, Number, _Property) ->
    {Server, _} = mathsserver:start(),
    ?LOG("Maths ~p", [Server]),
    exercise(factorial, Server, Number),
    
    mathsserver:stop(Server);
factorial(seq, Number, Property) ->
    
    Monitor = seq_main_mon:start_server(),
    fail_server:start_fail_server(fail_callback()),
    
    
    {Server, Workers} = mathsserver:start(),
    ?LOG("Maths ~p", [Server]),
    seq_main_mon:monitor_process(Monitor, [Server | Workers], true),
    seq_main_mon:start_property(Monitor, Property),
    
    exercise(factorial, Server, Number),
    
    mathsserver:stop(Server),
    notify_exercise_finished(Monitor),
    wait_fail_stopped(1);
factorial(seq_forest, Number, Property) ->
    
    fail_server:start_fail_server(fail_callback()),
    {Server, Workers} = mathsserver:start(),
    
    MonitorsForest = seq_main_mon:start_foreach(Workers, Property),
    
    exercise(factorial, Server, Number),
    
    mathsserver:stop(Server),
    lists:foreach(fun(MonPid) -> notify_exercise_finished(MonPid) end, MonitorsForest),
    wait_fail_stopped(length(MonitorsForest));
factorial(conc, Number, Property) ->
    
    Monitor = conc_main_mon:start_server(),
    fail_server:start_fail_server(fail_callback()),
    
%%     ?LOG("Conc MON ~p", [Monitor]),
    {Server, Workers} = mathsserver:start(),
    ?LOG("Maths ~p", [Server]),
    conc_main_mon:monitor_process(Monitor, [Server | Workers], true),
    conc_main_mon:start_property(Monitor, Property, [Server]),
    
    exercise(factorial, Server, Number),
    
    mathsserver:stop(Server),
    notify_exercise_finished(Monitor),
    wait_fail_stopped(1);
factorial(conc_forest, Number, Property) ->
    
    fail_server:start_fail_server(fail_callback()),
    {Server, Workers} = mathsserver:start(),
    
    MonitorsForest = conc_main_mon:start_foreach(Workers, Property, [Server]),
    
    exercise(factorial, Server, Number),
    
    mathsserver:stop(Server),
    lists:foreach(fun(MonPid) -> notify_exercise_finished(MonPid) end, MonitorsForest),
    wait_fail_stopped(length(MonitorsForest)).


summation(nomon, Number, _Property) ->
    {Server, _} = mathsserver:start(),
    ?LOG("Maths ~p", [Server]),
    spawn(fun() -> m_utils:callNTimes(Number, fun() -> self() ! test end) end),
    exercise(summation, Server, Number),
    
    mathsserver:stop(Server);
summation(seq, Number, Property) ->
    
    Monitor = seq_main_mon:start_server(),
    fail_server:start_fail_server(fail_callback()),
    
    {Server, Workers} = mathsserver:start(),
    ?LOG("Maths ~p", [Server]),
    seq_main_mon:monitor_process(Monitor, [Server | Workers], true),
    seq_main_mon:start_property(Monitor, Property),
    
    exercise(summation, Server, Number),
    
    notify_exercise_finished(Monitor),
    mathsserver:stop(Server),
    wait_fail_stopped(1);
summation(conc, Number, Property) ->
    Monitor = conc_main_mon:start_server(),
    fail_server:start_fail_server(fail_callback()),
    
    {Server, Workers} = mathsserver:start(),
    ?LOG("Maths ~p", [Server]),
    conc_main_mon:monitor_process(Monitor, [Server | Workers], true),
    conc_main_mon:start_property(Monitor, Property, [Server]),
    
    exercise(summation, Server, Number),
    
    mathsserver:stop(Server),
    notify_exercise_finished(Monitor),
    wait_fail_stopped(1).



%% ====================================================================
%% Internal functions
%% ====================================================================


