-module (element_textboxlist).
-compile(export_all).
-include_lib("wf.hrl").

reflect() -> record_info(fields, textboxlist).

render_element(R = #textboxlist{}) ->
  Id = case R#textboxlist.html_id of
    [] -> wf:temp_id();
    I -> I
  end,
  Anchor = R#textboxlist.anchor,
  Delegate = R#textboxlist.delegate,
  Tag = R#textboxlist.tag,
  Postback = wf_event:serialize_event_context({textboxlist_event, Delegate, Tag}, Anchor, undefined, ?MODULE),
  Values = R#textboxlist.values,
  QueryRemote = R#textboxlist.queryRemote,

  Plugins = [case R#textboxlist.autocomplete of
    false -> [];
    true -> {autocomplete,
      {struct,[
        {minLenght, R#textboxlist.minLenght},
        {queryRemote, QueryRemote},
        {onlyFromValues, R#textboxlist.onlyFromValues},
        case QueryRemote of
          false -> [];
          true -> {remote, [{postback, Postback}]}
        end
      ]}
    }
  end],

  S = wf:f(
    "$(function(){"++
    "var t = new $.TextboxList('#~s',"++
      "{unique: ~s,"++
        "startEditableBit:false,"++
        "inBetweenEditableBits:false,"++
        "plugins: ~s"++
    "});"++
    "var init = '~s';"++
    "if(init.length>0){"++
      "var vals = init.split(',');"++
      "var i, val;"++
      "for(i in vals){"++
        "val = vals[i];"++
        "t.add(val, val, val);"++
      "}"++
    "}"++
    "});"
  , [Id, R#textboxlist.unique, mochijson2:encode({struct, lists:flatten(Plugins)}), string:join(Values, ",")]),
  wf:wire(#script{script=S}),

  wf_tags:emit_tag(input, [
    {id, Id},
    {type, text},
    {class, [textboxlist, R#textboxlist.class]},
    {style, [R#textboxlist.style, "display:none;"]}
  ]).

event({textboxlist_event, Delegate, Tag})->
  SearchTerm = wf:q("search"),
  wf_context:type(first_request),
  wf:content_type("application/json"),
  Module = wf:coalesce([Delegate, wf:page_module()]),
  wf_context:data([
      Module:textboxlist_event(SearchTerm, Tag)
  ]).
