%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Logger module for hubberl's activities.

-module(hubberl_log).

-compile(export_all).

%% @spec log_request(Method, Path, DocRoot) -> ok
%% @doc log informations about request
log_request(Method, Path, DocRoot) ->
	io:format("=LOG REPORT===~n"),
	io:format("   {request,~n"),
	io:format("      Method = ~s,~n",  [Method]),
	io:format("      Path = ~s,~n",    [Path]),
	io:format("      DocRoot = ~s}~n", [DocRoot]).
