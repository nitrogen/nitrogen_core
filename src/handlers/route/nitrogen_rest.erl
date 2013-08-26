%% Nitrogen's RESTful Behaviour
%%
%% Enable this on a nitrogen module using:
%% -behaviour(nitrogen_rest).
%%
%% Then, add each of the callback methods (get/1, put/1, post/1, etc).
%% Which return either a body, or return {StatusCode, body}
%%
%% The HTTP methods and their meanings can be read about here:
%% http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
%%
%% NOTE: This is designed to work with the standard dynamic_route_handler
%% provided with Nitrogen. If you switch away from using the
%% dynamic_route_handler, then you'll have to modify whatever route handler you
%% do use to incorporate this module (if you wish to use this for handing REST,
%% which is certainly not required).

-module(nitrogen_rest).
-include("wf.hrl").
-export([
	handle_request/1,
	handle_return/1
]).

%% The return value of restful function can either be {StatusCode, Body} or
%% just a Body. If just Body is returned, then the status code will be 200, if
%% no other status code is specified, otherwise it will use 
-type restful_return() :: body() | {StatusCode :: integer(), Body :: body()}.

-callback get(    	PathInfo :: string()) -> restful_return().
-callback post(   	PathInfo :: string()) -> restful_return().
-callback put(    	PathInfo :: string()) -> restful_return().
-callback connect(	PathInfo :: string()) -> restful_return().
-callback head(   	PathInfo :: string()) -> restful_return().
-callback trace(  	PathInfo :: string()) -> restful_return().
-callback options(	PathInfo :: string()) -> restful_return().
-callback delete( 	PathInfo :: string()) -> restful_return().


-spec handle_request(Module :: module()) -> body().
% @doc This can be directed to in a route_handler, and can be used to dispatch
% restful requests to a module with the appropriately defined REST functions
% (get, put, etc), and will ultimately set the status code and return just a
% body which can be rendered.
handle_request(Module) ->
	PathInfo = wf:path_info(),
	Method = wf:request_method(),
	RestfulReturn = case erlang:function_exported(Module, Method, 1) of
		true ->
			Module:Method(PathInfo);
		false ->
			{405, ["This resource does not support the action '",wf:to_list(Method),"'"]}
	end,
	handle_return(RestfulReturn).

-spec handle_return(restful_return()) -> body().
handle_return({StatusCode, Body}) when is_integer(StatusCode) ->
	wf:status_code(StatusCode),
	Body;
handle_return(Body) ->
	Body.
