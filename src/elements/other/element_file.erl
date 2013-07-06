% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_file).
-include_lib ("wf.hrl").
-export([
    reflect/0,
    transform_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, file).

-spec transform_element(nitrogen_element()) -> nitrogen_element() | binary() | string().
transform_element(Record) -> 
    FilePath = Record#file.file,
    Encode = Record#file.html_encode,
    FileContents = case file:read_file(FilePath) of
        {ok, B} -> 
            wf:html_encode(B, Encode);
        _ -> 
            ?LOG("Error reading file: ~s~n", [FilePath]),
            wf:f("File not found: ~s.", [FilePath])
    end,

    case Record#file.include_panel of
        false ->
            FileContents;
        true ->
            #panel {
                id=Record#file.id,
                class=Record#file.class,
                html_id=Record#file.html_id,
                body=FileContents
            }
    end.
