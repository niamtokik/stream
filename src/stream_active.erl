%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2018, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(stream_active).
-behaviour(gen_statem).
-export([callback_mode/0]).
-export([handle_event/4]).

callback_mode() ->
    handle_event_function.

code_change(_,_,_,_) ->
    ok.

terminate(_,_,_) ->
    ok.

handle_event(Event, EventContent, _, Data) ->
    {keep_state, Data}.
