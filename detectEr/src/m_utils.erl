%% Author: Aldrin
%% Created: Feb 11, 2012
%% Description: General monitoring utils
-module(m_utils).

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
-export([timedateFormat/0, get_timestamp/0, write_log/4, setnth/3,
         average/1, profile/3, callNTimes/2,
         startMonitor/2, send_message/2, resolve_key/0, isSystemProcess/1]).

% API Functions

%% m_utils:timedateFormat().
timedateFormat() ->
	{{Year,Month,Day}, {Hr, Min, Sec}} = erlang:localtime(),
	FmtTimeDate = io_lib:format("~B/~B/~4..0B ~2B:~2.10.0B:~2.10.0B", [Day,Month,Year, Hr, Min, Sec]),
	lists:flatten(FmtTimeDate).

% gets a timestamp in ms from the epoch -- http://snipplr.com/view.php?codeview&id=23910
get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.


profile(M,F,A) ->

    statistics(runtime),

    apply(M,F,A),

    {_, Time1} = statistics(runtime),
    Time1.

average(X) -> average(X, 0, 0).
    
    average([H|T], Length, Sum) ->
        average(T, Length + 1, Sum + H);
    average([], Length, Sum) ->
        Sum / Length.

callNTimes(0, _) ->
    [];
callNTimes(N, Fun) when is_function(Fun) ->
    Result = Fun(),
    [Result | callNTimes(N-1, Fun)];
callNTimes(N, {M,F,A}) ->
    Result = apply(M,F,A),
    [Result | callNTimes(N-1, {M,F,A})].


write_log(Module, Line, X, Y) ->
	file:write_file("detecter.log", 
					io_lib:fwrite("{~p,~p,~p,~p}: ~s~n", 
								  [timedateFormat(), Module, Line, self(), io_lib:format(X,Y)]), [append]).


startMonitor(Mon, Monitors) when is_function(Mon) ->
    spawn(fun () -> Mon(Monitors) end).

send_message(MonPids, Msg) ->
    case MonPids of
        [] ->
            ok;
        [Pid | Pids] ->
            Pid ! Msg,
            send_message(Pids, Msg)
    end.

%% http://stackoverflow.com/questions/4776033/how-to-change-an-element-in-a-list-in-erlang
%% setnth(Index, List, NewElement) -> List.
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

resolve_key() ->
	fun(F, Key, List) ->
			case List of
				[] ->
					not_found;
				[{Key, Value} | _Other] ->
					Value;
				[ _ | Other] ->
					F(F, Key, Other)
			end
	end.
	
isSystemProcess(Pid) when is_pid(Pid) ->
    lists:member(Pid, [whereis(P) || P <- systemProcesses()]);
isSystemProcess(Pid) ->
    lists:member(Pid, systemProcesses()).

systemProcesses() -> 
	[code_server, kernel_safe_sup 
%% 	 user, 
%% 	 error_logger].
	].