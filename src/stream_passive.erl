%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2017, Mathieu Kerjouan
%%% @doc = Passive Stream FSM =
%%%      
%%%      This FSM manage only input stream and doesn't have to
%%%      maintain state about external stream generator.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stream_passive).
-behaviour(gen_statem).
-compile([export_all]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
    handle_event_function.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
code_change(_,_,_,_) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
terminate(_,_,_) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_event(cast, Event, _, Data) ->
    handle_cast(Event, Data);
handle_event({call, From}, Event, _, Data) ->
    handle_call(From, Event, Data);
handle_event(info, Event, _, Data) ->
    handle_info(Event, Data).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_cast({input, Stream}, Data) ->
    stream_lib:input(Data, Stream);

handle_cast({action, {Size, Shift, Pattern, Function}}, Data) ->
    stream_lib:action(Data, {Size, Shift, Pattern, Function});
handle_cast({action, {Size, Shift, Pattern, Function, Opts}}, Data) ->
    stream_lib:action(Data, {Size, Shift, Pattern, Function, Opts});

handle_cast({search, {Pattern, Function}}, Data) ->
    stream_lib:search(Data, {Pattern, Function});
handle_cast({search, {Pattern, Function, Opts}}, Data) ->
    stream_lib:search(Data, {Pattern, Function, Opts});

handle_cast({parse, {Module, Function, Arguments}}, Data) ->
    stream_lib:parse(Data, {Module, Function, Arguments});
handle_cast({parse, {Module, Function, Arguments, Opts}}, Data) ->
    stream_lib:parse(Data, {Module, Function, Arguments, Opts});
handle_cast(_Event, Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_call(From, {cut, {Size, Shift}}, Data) ->
    stream_lib:cut(Data, From, {Size, Shift});
handle_call(From, {cut, {Size, Shift, Opts}}, Data) ->
    stream_lib:cut(Data, From, {Size, Shift, Opts});

handle_call(From, {copy, {Size, Shift}}, Data) ->
    stream_lib:copy(Data, From, {Size, Shift});
handle_call(From, {copy, {Size, Shift, Opts}}, Data) ->
    stream_lib:copy(Data, From, {Size, Shift, Opts});

handle_call(From, {map, {Size, Shift, Function}}, Data) ->
    stream_lib:map(Data, From, {Size, Shift, Function});
handle_call(From, {map, {Size, Shift, Function, Opts}}, Data) ->
    stream_lib:map(Data, From, {Size, Shift, Function, Opts});

handle_call(From, _Event, Data) ->
    {keep_state, Data, [{reply, From, ok}]}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_info(_Event, Data) ->
    {keep_state, Data}.
