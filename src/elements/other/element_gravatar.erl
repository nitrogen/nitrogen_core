% vim: sw=4 ts=4 et ft=erlang
-module (element_gravatar).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, gravatar).

-spec render_element(#gravatar{}) -> body().
render_element(Record) -> 
    Image = #image {
        html_id=Record#gravatar.html_id,
        id=Record#gravatar.id,
        title=Record#gravatar.title,
        anchor=Record#gravatar.anchor,
        data_fields=Record#gravatar.data_fields,
        image = gravatar_icon(Record)
    },
    element_image:render_element(Image).

gravatar_icon(G = #gravatar{size=Size}) when is_integer(Size) ->
	gravatar_icon(G#gravatar{size=wf:to_list(Size)});
gravatar_icon(#gravatar{email=Email, size=Size, rating=Rating, default=Default}) ->
    GravatarId = digest2str(erlang:md5(wf:clean_lower(Email))),
    wf:f("http://www.gravatar.com/avatar/~s?size=~s&r=~s&d=~s" ,
        [GravatarId, Size, Rating, Default]).

digest2str(Digest) ->
    [[nibble2hex(X bsr 4), nibble2hex(X band 15)] ||
    X <- binary_to_list(Digest)].

-define(IN(X,Min,Max), X >= Min, X =< Max).
nibble2hex(X) when ?IN(X, 0, 9)   -> X + $0;
nibble2hex(X) when ?IN(X, 10, 15) -> X - 10 + $a.
