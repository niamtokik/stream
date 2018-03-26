%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2018, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------
-module(stream_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
    [ stream_create
    , stream_copy
    , stream_cut
    , stream_map
    , stream_action
    , stream_parse ].

init_per_suite(Config) ->
    [].

end_per_suite(Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    {ok, Stream} = stream:start(),
    {ok, Timer} = stream:timer(Stream, 1000, <<"thisisatest">>),
    [{stream, Stream}, {timer, Timer}].

end_per_testcase(_Case, Config) ->
    Stream = proplists:get_value(stream, Config),
    Timer = proplists:get_value(timer, Config),
    timer:cancel(Timer),
    stream:stop(Stream),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stream_create() ->
    [].
stream_create(Config) ->
    Stream = proplists:get_value(config, Config),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stream_copy() ->
    [].
stream_copy(Config) ->
    Stream = proplists:get_value(stream, Config),
    { <<"t">>, Rest } = stream:copy(Stream, 8, 0),
    { <<"h">>, Rest2 } = Rest(),
    { <<"i">>, _Rest3 } = Rest2().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stream_cut() ->
    [].
stream_cut(Config) ->
    Stream = proplists:get_value(stream, Config),
    { <<"t">>, Rest } = stream:cut(Stream, 8, 0),
    { <<"h">>, Rest2 } = Rest(),
    { <<"i">>, _Rest3 } = Rest2().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stream_map() ->
    [].
stream_map(Config) ->
    Stream = proplists:get_value(stream, Config),
    Fun = fun F(<<"t">>) -> true;
	      F(<<"h">>) -> true;
	      F(_) -> false
	  end,
    { true, Rest } = stream:map(Stream, 8, 0, Fun),
    { true, Rest2 } = Rest(),
    { false, _Rest3 } = Rest2().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stream_action() ->
    [].
stream_action(Config) ->
    Stream = proplists:get_value(stream, Config),
    Action = fun A(<<"this", _/bitstring>>) -> found;
		 A(_) -> ok 
	     end,
    stream:action(Stream, Action).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stream_parse() ->
    [].
stream_parse(Config) ->
    Stream = proplists:get_value(stream, Config),
    Parser = ok.
