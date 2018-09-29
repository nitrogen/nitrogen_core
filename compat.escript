#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

main([]) ->
    crypto:start(),
    code:ensure_loaded(rand),

	Filename = "include/compat.hrl",
	io:format("Generating compatibility macros...\n"),
	Encrypt = encrypt(),
	Decrypt = decrypt(),
	Hash = hash(),
    Unique = unique(),
    RandUniform1 = rand_uniform_1(),
    RandUniform2 = rand_uniform_2(),

	io:format("...?WF_ENCRYPT will use: ~p~n",[Encrypt]),
	io:format("...?WF_DECRYPT will use: ~p~n",[Decrypt]),
	io:format("...?WF_HASH will use:    ~p~n",[Hash]),
    io:format("...?WF_UNIQUE will use:  ~p~n",[Unique]),
    io:format("...?WF_RAND_UNIFORM/1 will use: ~p~n",[RandUniform1]),
    io:format("...?WF_RAND_UNIFORM/2 will use: ~p~n",[RandUniform2]),


	Contents = [
		"-define(WF_ENCRYPT(Key, IV, Data), ",Encrypt,").\n",
		"-define(WF_DECRYPT(Key, IV, Data), ",Decrypt,").\n",
		"-define(WF_HASH(Data), ",Hash,").\n",
        "-define(WF_UNIQUE, ",Unique,").\n",
        "-define(WF_RAND_UNIFORM(Max), ",RandUniform1,").\n",
        "-define(WF_RAND_UNIFORM(Min,Max), ",RandUniform2,").\n"
	],

    ContentsBin = iolist_to_binary(Contents),
    case file:read_file(Filename) of
        {ok, ContentsBin} -> 
            io:format("...no changes needed to ~p. Skipping writing new file\n",[Filename]);
        _ -> 
            io:format("...writing ~p\n",[Filename]),
            file:write_file(Filename, Contents)
    end.

rand_uniform_1() ->
    case erlang:function_exported(rand, uniform, 1) of
        true ->
            "rand:uniform(Max)";
        false ->
            "crypto:rand_uniform(1, Max)"
    end.

rand_uniform_2() ->
    case erlang:function_exported(rand, uniform, 1) of
        true ->
            "(rand:uniform(1, Max-Min+1)+Min-1)";
        false ->
            "crypto:rand_uniform(Min, Max)"
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
            "begin {_,S,US}=erlang:now(), S*1000000+US end"
    end.
