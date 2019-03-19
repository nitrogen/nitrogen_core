% vim: sw=4 ts=4 et
-module(nitrogen_dev).
-export([
    command/1,
    help/0,
    compile/0,
    page/1,
    action/1,
    element/1,
    plugin/1
]).

help() ->
    Message = [
        "   ---------------------------------------------------------",
        "                 Nitrogen Developer Utility",
        "   ---------------------------------------------------------",
        "   ",
        "   nitrogen_dev:compile()",
        "     - Recompile changed files on a running Nitrogen system.",
        "   ",
        "   nitrogen_dev:page(Name)",
        "     - Create a new page with the specified name.",
        "   ",
        "   nitrogen_dev:action(Name)",
        "     - Create a new action with the specified name.",
        "   ",
        "   nitrogen_dev:element(Name)",
        "     - Create a new element with the specified name.",
        "   ",
        "   nitrogen_dev:help()",
        "     - This screen."
    ],
    [io:format("~s~n", [X]) || X <- Message],
    ok.

compile() ->
    command(["compile"]).

page(Name) ->
    command(["page", wf:to_list(Name)]).

action(Name) ->
    command(["action", wf:to_list(Name)]).

element(Name) ->
    command(["element", wf:to_list(Name)]).

plugin(Name) ->
    command(["plugin", wf:to_list(Name)]).

command([]) ->
    Message = [
        "   ---------------------------------------------------------",
        "                 Nitrogen Developer Utility",
        "   ---------------------------------------------------------",
        "   ",
        "   ./bin/dev compile",
        "     - Recompile changed files on a running Nitrogen system.",
        "   ",
        "   ./bin/dev page <name>",
        "     - Create a new page with the specified name.",
        "   ",
        "   ./bin/dev action <name>",
        "     - Create a new action with the specified name.",
        "   ",
        "   ./bin/dev element <name>",
        "     - Create a new element with the specified name.",
        "   ",
        "   ./bin/dev plugin <name>",
        "     - Create a new plugin with the specified name.",
        "   ",
        "   ./bin/dev help",
        "     - This screen."
    ],
    [io:format("~s~n", [X]) || X <- Message];

command(["compile"]) ->
    sync:go();

command(["page", Name]) ->
    Filename = filename:join([".", "site", "src", Name ++ ".erl"]),
    Template = filename:join([".", "site", ".prototypes", "page.erl"]),
    case filelib:is_file(Filename) of
        true ->
            io:format("File already exists: " ++ Filename ++ "!\n");
        false ->
            Contents = template(Template, Name),
            file:write_file(Filename, Contents),
            io:format("Created page: " ++ Filename ++ "\n"),
            io:format("Remember to recompile!~n")
    end;

command(["element", Name]) ->
    Filename = filename:join([".", "site", "src", "elements", "element_" ++ Name ++ ".erl"]),
    Template = filename:join([".", "site", ".prototypes", "element.erl"]),
    case filelib:is_file(Filename) of
        true ->
            io:format("File already exists: " ++ Filename ++ "!\n");
        false ->
            Contents = template(Template, Name),
            file:write_file(Filename, Contents),
            io:format("Created element: " ++ Filename ++ "\n"),
            io:format("Remember to recompile!~n")
    end;

command(["action", Name]) ->
    Filename = filename:join([".", "site", "src", "actions", "action_" ++ Name ++ ".erl"]),
    Template = filename:join([".", "site", ".prototypes", "action.erl"]),
    case filelib:is_file(Filename) of
        true ->
            io:format("File already exists: " ++ Filename ++ "!\n");
        false ->
            Contents = template(Template, Name),
            file:write_file(Filename, Contents),
            io:format("Created action: " ++ Filename ++ "\n"),
            io:format("Remember to recompile!~n")
    end;

command(["plugin", Name]) ->
    Dir = filename:join([".", "lib", Name]),
    PluginTar = filename:join([".", "site", ".prototypes", "plugin.tgz"]),
    TemplateApp = filename:join([".", "site", ".prototypes", "plugin.app.src"]),
    App = filename:join([".", "lib", Name, "src",
        io_lib:format("~s.app.src", [Name])]),
    TemplateDemo = filename:join([".", "site", ".prototypes", "plugin_page.erl"]),
    Demo = filename:join([".", "site", "src", Name ++ ".erl"]),

    case {filelib:is_dir(Dir), filelib:is_file(Demo)} of
        {true, _} ->
            io:format("Directory already exists: " ++ Dir ++ "!\n");
        {_, true} ->
            io:format("File for the plugin demo page already exists: " ++ Demo ++ "!\n");
        {false, false} ->
            ok = file:make_dir(Dir),
            erl_tar:extract(PluginTar, [{cwd,Dir}, compressed]),
            file:write_file(App, template(TemplateApp, Name)),
            file:write_file(Demo, template(TemplateDemo, Name)),

            io:format("Plugin created in ~s (demo page in ~s)\n\n", [Dir, Demo]),
            io:format([
                "Don't forget to: \n\n",
                "1) initialize your newly created plugin with version control (hg or git)\n",
                "2) clone your plugin outside this nitrogen release and update the 'deps' section in your rebar.config. For example:\n\n"]),
            io:format(
                "   {~s, \".*\", {hg, \"https://bitbucket.org/user/my_plugin\", default}}\n\n",
                [Name]),
            io:format(
                "3) recompile and check the demo page /~s\n", [Name])
    end;

command(["help"|_]) ->
    command([]);

command(Other) ->
    io:format("ERROR: Unknown command: ~p~n~n", [Other]),
    command([]).

template(Template, Name) ->
    {ok, B} = file:read_file(Template),
    replace_name(binary_to_list(B), Name).

replace_name(S, Name) ->
    case S of
        "[[[NAME]]]" ++ Rest ->
            [Name|replace_name(Rest, Name)];
        [H|T] ->
            [H|replace_name(T, Name)];
        [] ->
            []
    end.
