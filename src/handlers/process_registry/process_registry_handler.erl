% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

%
% The process_registry handler allows you to associate a process with
% a key and later retrieve the process.
%
% Next Steps 
% ----------
% - Create an implementation using Ulf Wiger's "gproc" project.
%   http://www.erlang.se/workshop/2007/proceedings/02wiger.pdf
%   http://svn.ulf.wiger.net/gproc/
%

-module (process_registry_handler).
-include("wf.hrl").
-export ([
    get_pid/1,
    get_pid/2
]).

-callback init(         handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback finish(       handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback get_pid(      Key :: term(),
                        handler_config(),
                        handler_state()) -> {ok, pid() | undefined, handler_state()}.
-callback get_pid(      Key :: term(),
                        Function :: fun(),
                        handler_config(),
                        handler_state()) -> {ok, pid(), handler_state()}.

% Get the process associated with this Key.
-spec get_pid(Key :: term()) -> {ok, pid()} | undefined.
get_pid(Key) ->
    case wf_handler:call(process_registry_handler, get_pid, [Key]) of
        {ok, undefined} -> undefined;
        {ok, Pid} -> {ok, Pid}
    end.

% Return the process associated with Key. If that process does not
% exist, then create a new process and associate it with Key.
-spec get_pid(Key :: term(), Function :: fun()) -> {ok, pid()}.
get_pid(Key, Function) ->
    {ok, _Pid} = wf_handler:call(process_registry_handler, get_pid, [Key, Function]).


