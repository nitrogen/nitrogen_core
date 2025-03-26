% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(default_secret_handler).
-include("wf.hrl").
-behaviour(secret_handler).
-export ([
    init/2,
    finish/2,
    get_value/4
]).

-define(FILE_KEY, file).
-define(SECRETS_FILE_KEY, secrets_file).
-define(CACHE_NAME, ?MODULE).

%% default cache_ttl = 1000ms
-record(state, {
    file,
    cache_contents_ttl=1000,
    cache_filename_ttl=1000
}).

verify_state(State = #state{file=Blank,
                            cache_filename_ttl=FilenameTTL})
        when ?WF_BLANK(Blank) ->
    verify_state(State#state{file=get_secrets_filename(FilenameTTL)});
verify_state(State) ->
    State.

get_secrets_filename(FilenameTTL) ->
    nitro_cache:get(?CACHE_NAME, FilenameTTL, ?SECRETS_FILE_KEY, fun default_secrets_file/0).

default_secrets_file() ->
    App = app_name(),
    {ok, [[Home]]} = init:get_argument(home),
    NitrogenDir = ".nitrogen",
    Filename = wf:to_list(App) ++ ".config",
    filename:join([Home, NitrogenDir, Filename]).

app_name() ->
    {ok, App} = application:get_application(),
    App.

process_config(Config) ->
    process_config(ds:to_list(Config), #{}).

process_config([], State) ->
    State;
process_config([{Key, Val} | T], State0) ->
    State = process_config_value(Key, Val, State0),
    process_config(T, State).

process_config_value(?FILE_KEY, Val, State) ->
    State#state{file=Val};
process_config_value(Key, _Val, _) ->
    erlang:exit({error, {invalid_config_key, [{module, ?MODULE}, {key, Key}]}}).

init(Config, _State) ->
    State1 = process_config(Config),
    State2 = verify_state(State1),
    {ok, State2}.

finish(_Config, _State) ->
    {ok, []}.

get_value(Key, Default, _Config, State) ->
    Vals = get_all_values(State),
    ds:get(Vals, Key, Default).

get_all_values(#state{file=File, cache_contents_ttl=ContentsTTL}) ->
    LookupFun = fun() ->
        case file:consult(File) of
            {ok, Body} ->
                Body;
            {error, enoent} ->
                logger:error("Attempting to read secrets from non-existant file: ~ts.~nPlease ensure that file exists and is in a format readable by file:consult/1.", [File]),
                {error, enoent}
        end
    end,
    nitro_cache:get(?CACHE_NAME, ContentsTTL, {nitrogen_secret_handler, File}, LookupFun).
