#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

main([]) ->
    crypto:start(),

	Filename = "include/compat.hrl",
	io:format("Generating compatibility macros...\n"),
	Encrypt = encrypt(),
	Decrypt = decrypt(),
	Hash = hash(),
    Unique = unique(),

	io:format("...?WF_ENCRYPT will use: ~p~n",[Encrypt]),
	io:format("...?WF_DECRYPT will use: ~p~n",[Decrypt]),
	io:format("...?WF_HASH will use:    ~p~n",[Hash]),
    io:format("...?WF_UNIQUE will use:  ~p~n",[Unique]),

	Contents = [
		"-define(WF_ENCRYPT(Key, IV, Data), ",Encrypt,").\n",
		"-define(WF_DECRYPT(Key, IV, Data), ",Decrypt,").\n",
		"-define(WF_HASH(Data), ",Hash,").\n",
        "-define(WF_UNIQUE, ",Unique,").\n"
	],

    ContentsBin = iolist_to_binary(Contents),
    case file:read_file(Filename) of
        {ok, ContentsBin} -> 
            io:format("...no changes needed to ~p. Skipping writing new file\n",[Filename]);
        _ -> 
            io:format("...writing ~p\n",[Filename]),
            file:write_file(Filename, Contents)
    end.


encrypt() ->
	case erlang:function_exported(crypto, block_encrypt, 4) of
		true ->
			"crypto:block_encrypt(aes_cbc128, Key, IV, Data)";
		false ->
			"crypto:aes_cbc_128_encrypt(Key, IV, Data)"
	end.

decrypt() ->
	case erlang:function_exported(crypto, block_decrypt, 4) of
		true ->
			"crypto:block_decrypt(aes_cbc128, Key, IV, Data)";
		false ->
			"crypto:aes_cbc_128_decrypt(Key, IV, Data)"
	end.

hash() ->
	case erlang:function_exported(crypto, hash, 2) of
		true ->
			"crypto:hash(sha, Data)";
		false ->
			"crypto:sha(Data)"
	end.

unique() ->
    case erlang:function_exported(erlang, unique_integer, 1) of
        true ->
            "erlang:unique_integer([positive])";
        false ->
            "begin {_,S,US}=erlang:now(), S*1000,+US end"
    end.
