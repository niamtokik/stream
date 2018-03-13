%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2017, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------
-module(stream_passive).
% -behaviour(gen_statem).
-compile([export_all]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
passive(cast, Event, Data) ->
    handle_cast(Event, Data);
passive({call, From}, Event, Data) ->
    handle_call(From, Event, Data);
passive(info, Event, Data) ->
    handle_info(Event, Data).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast(_Event, Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call(From, _Event, Data) ->
    {keep_state, Data, [{reply, From, ok}]}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info(_Event, Data) ->
    {keep_state, Data}.




