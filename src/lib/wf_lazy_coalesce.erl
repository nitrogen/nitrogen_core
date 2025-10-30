-module(wf_lazy_coalesce).
-export([parse_transform/2]).

%% This module is the parse transform for the lazy_coalesce process

parse_transform(Forms, _Options) ->
    %dump_forms("lazy.forms", Forms),
    Forms2 = process_forms(Forms),
    %dump_forms("lazy.newforms", Forms2),
    Forms2.

%dump_forms(File, Forms) when is_list(Forms) ->
%    FormattedForms = [format_forms(F) || F <- Forms],
%    file:write_file(File, FormattedForms).
%
%format_forms(X) ->
%    io_lib:format("~p.~n~n", [X]).

process_forms(Forms) when is_list(Forms) ->
    [process_forms(F) || F <- Forms];
process_forms(Form) when is_tuple(Form) ->
    case process_form(Form) of
        {ok, NewForm} ->
            NewForm;
        no_change ->
            List = tuple_to_list(Form),
            ProcessedList = process_forms(List),
            list_to_tuple(ProcessedList)
    end;
process_forms(Form) ->
    Form.

process_form({call, LN, {remote, LN2, {atom, LN3, wf}, {atom, LN4, lazy_coalesce}}, [Args = {cons, _, _, _}] }) ->
    WrappedArgs = wrap_args(Args),
    %io:format("Wrapping wf:lazy_coalesce on ~p~n",[LN4]),
    NewForm = {call, LN, {remote, LN2, {atom, LN3, wf}, {atom, LN4, eval_coalesce}}, [WrappedArgs] },
    {ok, NewForm};
process_form({call, LN, {remote, LN2, {atom, LN3, wf}, {atom, LN4, lazy_coalesce}}, Args = [_] }) ->
    {Line, Col} = case LN of
        {L, C} -> {L, C};
        L -> {L, 0}
    end,
    logger:warning("Call to wf:lazy_coalesce at line ~p (column ~p) is not explicitly listing each argument.~nThis cannot be made lazy. Converting it to the plain wf:coalesce/1~n", [Line, Col]),
    NewForm = {call, LN, {remote, LN2, {atom, LN3, wf}, {atom, LN4, coalesce}}, Args },
    {ok, NewForm};
process_form(_X) ->
    no_change.
    %% if it's not one of the above things, then dive into it to process it.
    %{no_change, X}.

wrap_args({cons, LN, H, T}) ->
    %io:format("Found a cons...~n"),
    H2 = wrap_arg(H),
    T2 = wrap_args(T),
    {cons, LN, H2, T2};
wrap_args(Nil = {nil, _}) ->
    %io:format("End of List~n"),
    Nil.

wrap_arg(Arg = {Type, _, _}) when Type==var;
                            Type==atom;
                            Type==integer;
                            Type==string;
                            Type==binary;
                            Type==iolist ->
    %% This is a simple term, we can safely pass it through, as wrapping a Fun around it would be more overhead for the VM during runtime
    %io:format("Simple Arg (~p). Nothing to format...~n", [Arg]),
    Arg;
wrap_arg(Arg) when is_tuple(Arg) ->
    LN = element(2, Arg),
    ClauseArgs = [],
    Guards = [],
    Clause = {clause, LN, ClauseArgs, Guards, [Arg]},
    %io:format("Wrapping Arg: ~p~n",[Arg]),
    WrappedArg = {'fun', LN, {clauses, [Clause]}},
    WrappedArg.
