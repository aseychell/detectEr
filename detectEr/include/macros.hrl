
-define(MAIN_MON, monitor_sup).
-define(FAIL_MON, fail_sup).

% REFERENCE: MC ERLANG 
-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p,~p,~p}: ~s~n", [m_utils:timedateFormat(), ?MODULE,?LINE, self(), io_lib:format(X,Y)])).
%% -define(LOG(X,Y), m_utils:write_log(?MODULE, ?LINE, X, Y)).
%% -define(LOG(X,Y), io:format("log ~n", [])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.
% /REFERENCE: MC ERLANG