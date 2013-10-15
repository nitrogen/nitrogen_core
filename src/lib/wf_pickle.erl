% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2013 Rusty Klophaus
% See MIT-LICENSE for licensing information.
% This is heavily inspired by the n2o_secret pickler located at:
% https://github.com/5HT/n2o/blob/master/src/handlers/n2o_secret.erl

-module (wf_pickle).
-author("Oleksandr Nikitin").
-include("wf.hrl").

-export ([
    pickle/1,
    depickle/1, 
    depickle/2
]).

-type pickled() :: binary() | string().

-spec pickle(Data :: term()) -> pickled().
pickle(Data) ->
    Message = term_to_binary({Data,os:timestamp()}),
    Padding = size(Message) rem 16,
    Bits = (16-Padding)*8,
    Key = signkey(),
    IV = crypto:rand_bytes(16),
    Cipher = ?WF_ENCRYPT(Key,IV,<<Message/binary,0:Bits>>),
    Signature = ?WF_HASH(<<Key/binary,Cipher/binary>>),
    modified_base64_encode(<<IV/binary,Signature/binary,Cipher/binary>>).

-spec depickle(PickledData :: binary() | string()) -> undefined | term().
depickle(PickledData) ->
    depickle(PickledData, infinity).

-spec depickle(PickledData :: pickled(), TTLSeconds :: infinity | integer()) -> undefined | term().
depickle(PickledData, TTLSeconds) ->
    try
        {Data, PickledTime} = inner_depickle(PickledData),
        case verify_depickle_time(PickledTime, TTLSeconds) of
            true -> Data;
            false -> undefined
        end
    catch _:_ -> undefined
    end.


%% PRIVATE FUNCTIONS

-spec verify_depickle_time(PickledTime :: erlang:timestamp(), TTLSeconds :: infinity | integer()) -> boolean().
verify_depickle_time(_PickledTime, infinity) ->
    %% Short circuit, even though any number always evaluated to less than infinity,
    %% But this means we don't have to call os:timestamp(), and do the timer comparison
    %% Minor performance improvement.
    true;
verify_depickle_time(PickledTime, TTLSeconds) ->
    AgeInSeconds = timer:now_diff(os:timestamp(), PickledTime) / 1024 / 1024,
    AgeInSeconds < TTLSeconds.

-spec inner_depickle(PickledData :: pickled()) -> {term(), erlang:timestamp()}.
inner_depickle(PickledData) ->
    Key = signkey(),
    Decoded = modified_base64_decode(wf:to_binary(PickledData)),
    <<IV:16/binary,Signature:20/binary,Cipher/binary>> = Decoded,
    Signature = ?WF_HASH(<<Key/binary,Cipher/binary>>),
    {_Data,_Time} = binary_to_term(?WF_DECRYPT(Key,IV,Cipher)).

-spec signkey() -> binary().
signkey() ->
    case config_handler:get_value(signkey) of
        undefined ->
            erlang:md5(wf:to_list(erlang:get_cookie()));
        Key -> 
            Key
    end.

-spec modified_base64_encode(binary()) -> binary().
% @doc Replace '+' and '/' with '-' and '_', respectively.  Strip '='.
% This is to ensure compatibility with mochiweb's "quote" checking for
% cookies, which is done so to prevent any conflicts with URL encoding
% schemes, and how browsers all treat cookie quoting differently.
%
% See https://github.com/mochi/mochiweb/blob/master/src/mochiweb_cookies.erl#L98
modified_base64_encode(B) -> m_b64_e(base64:encode(B), <<>>).
m_b64_e(<<>>, Acc) -> Acc;
m_b64_e(<<$+, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $->>);
m_b64_e(<<$/, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $_>>);
m_b64_e(<<$=, Rest/binary>>, Acc) -> m_b64_e(Rest, Acc);
m_b64_e(<<H,  Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, H>>).

-spec modified_base64_decode(binary()) -> binary().
% @doc Replace '-' and '_' with '+' and '/', respectively.  Pad with '=' to a
% multiple of 4 chars.
modified_base64_decode(B) -> base64:decode(m_b64_d(B, <<>>)).
m_b64_d(<<>>, Acc) when size(Acc) rem 4 == 0 -> Acc;
m_b64_d(<<>>, Acc) when size(Acc) rem 4 /= 0 -> m_b64_d(<<>>, <<Acc/binary, $=>>);
m_b64_d(<<$-, Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, $+>>);
m_b64_d(<<$_, Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, $/>>);
m_b64_d(<<H,  Rest/binary>>, Acc) -> m_b64_d(Rest, <<Acc/binary, H>>).
