-module(wf_pandoc).
-include("wf.hrl").
-export([
    convert_file/2,
    convert/2
]).

convert(Contents, Options)  ->
    ScratchDir = simple_bridge_util:get_scratch_dir("./scratch"),
    RandFile = filename:join(ScratchDir, "tempfile_" ++ wf:to_list(?WF_RAND_UNIFORM(1000000000000, 99999999999999999))),
    file:write_file(RandFile, Contents),
    Converted = convert_file(RandFile, Options),
    file:delete(RandFile),
    Converted.

convert_file(Src, Options) ->
    Cmd = [make_cmd(Options), " ", Src],
    Result = os:cmd(Cmd),
    wf:to_unicode_binary(Result).

make_cmd(Options) ->
    Opts = make_options(Options),
    ["pandoc ", Opts].

make_options(Options) when is_map(Options) ->
    make_options(maps:to_list(Options));
make_options(Options) when is_list(Options) ->
    Opts = [make_option(O) || O <- Options],
    lists:join(" ",Opts).

make_option({Key, Value}) ->
    [" --", wf:to_list(Key),"=",wf:to_list(Value)];
make_option(Option) ->
    [" --",wf:to_list(Option)].
