-module (element_youtube).
-compile(export_all).
-include("wf.hrl").

reflect() -> record_info(fields, youtube).

-define(RE_OPT,[{capture,all_but_first,binary}]).

-define(KEY_REGEXES,[
		%% Normal URL if copied straight from address bar
		"youtube\\.com/watch\\?v=([\\w\\-]+)",

		%% Youtube's URL shortener
		"youtu.be/([\\w\-]+)",

		%% Check if it's just a key
		"^([\\w-]+)$",

		%% Scan the whole thing looking for embed code (likely means user copied the embed code)
		"youtube.com/embed/([\\w\-]+)"
	]).


embed_url(Key) ->
	KeyBin = wf:to_binary(Key),
	<<"https://www.youtube.com/embed/",KeyBin/binary>>.	

extract_key(URL) ->
	extract_key(URL, ?KEY_REGEXES).

extract_key(_URL, []) ->
	not_found;
extract_key(URL, [RE|REs]) ->
	case re:run(URL, RE, ?RE_OPT) of
		{match, Key} ->
			Key;
		nomatch -> 
			extract_key(URL, REs)
	end.


render_element(#youtube{
				class=Class,
				width=Width,
				height=Height,
                data_fields=DataFields,
                aria=Aria,
				key=Key,
				allowfullscreen=Allowfullscreen }) ->

	%% We extract the real Key to be forgiving of bad inputs, such as users copying the whole URL
	RealKey = extract_key(Key),
	URL = embed_url(RealKey),
	wf_tags:emit_tag(iframe," ",[
		{class,Class},
		{width,wf:to_list(Width)},
		{height,wf:to_list(Height)},
		{src,URL},
		{frameborder,"0"},
        {data_fields, DataFields},
        {aria, Aria},
	    ?WF_IF(Allowfullscreen, {allowfullscreen})
	]).

