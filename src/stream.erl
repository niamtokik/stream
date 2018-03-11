%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018, Mathieu Kerjouan <contact [at] steepath [dot] eu>
%%% @version 0.1.0
%%% @title Infinite Bitstring Stream State Machine
%%% @doc = Introduction =
%%%
%%%      This code was mainly created to solve a problem of high
%%%      throughput and dynamic parsing over infinite transaction
%%%      stream.
%%%
%%%      stream library use standard OTP function to start and
%%%      stop process:
%%%
%%%      ``` {ok, Pid} = stream:start().
%%%      ```
%%%
%%%      In this example, we'll see how to simulate a flow
%%%      of random bytes coming from timer module. First thing
%%%      we need to start timer module:
%%%
%%%      ``` timer:start().
%%%      ```
%%%
%%%      We can now start our random byte generator. stream:timer/3
%%%      is a wrapper around timer:apply_interface/3 function, giving
%%%      us the possibility to call a function from any kind of module:
%%%
%%%      ``` {ok, Ref} = stream:timer(Pid, 1000, crypto:rand_bytes(10)).
%%%      ```
%%%
%%%      We can now see what we have currently on our stream buffer:
%%%
%%%      ``` {Stream, Next} = stream:copy(Pid, 8).
%%%      ```
%%%
%%%      Next is a lambda function which give us the possibility to
%%%      navigate through our buffer:
%%%
%%%      ``` {Stream2, Next2} = Next().
%%%          {Stream3, Next3} = Next2().
%%%      ```
%%%
%%%      copy function doesn't alter the current stream, and to
%%%      when you recall it with Next2 function, you'll be automatically
%%%      shifted based on the pattern size.
%%%
%%%      Alternatively, cut function alter the stream, cutting them in
%%%      parts.
%%%
%%%      ``` {Cut1, CNext} = cut(Pid, 8).
%%%          {Cut2, CNext2} = CNext().
%%%      ```
%%%
%%%      We can now stop our FSM.
%%%
%%%      ``` ok = stream:stop(Pid).
%%%      ```
%%% @end
%%%===================================================================
-module(stream).
-behaviour(gen_statem).
% generic OTP functions
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([stop/1]).
% gen_statem standard function
-export([callback_mode/0, init/1, code_change/4, terminate/3]).
% gen_statem states
-export([bit/3, byte/3, custom/3]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
% API
-export([call/2, call/3, cast/2, info/2]).
-export([cut/2, cut/3, cut/4]).
-export([input/2, input/3]).
-export([copy/2, copy/3, copy/4]).
-export([map/3, map/4, map/5]).
-export([timer/3, timer/4]).

-include_lib("eunit/include/eunit.hrl").
-record(state, { buffer = <<>> :: bitstring()
	       , mode = bit 
	       , size = 1
	       , insufficient = undefined :: term()
	       }).

%%--------------------------------------------------------------------
%% @doc wrappers around generic start functions.
%%
%%     ``` {ok, Pid} = stream:start().
%%         {ok, Pid2} = stream:start(StreamArgs).
%%         {ok, Pid3} = stream:start(StreamArgs, FSM_Opts).
%%     ```
%% @end
%%--------------------------------------------------------------------
-spec start() 
	   -> {ok, pid()}.
start() ->
    start([], []).

-spec start( Args :: list()) 
	   -> {ok, pid()}.
start(Args) ->
    start(Args, []).

-spec start( Args :: list()
	   , Opts :: list()) 
	   -> {ok, pid()}.
start(Args, Opts) ->
    gen_statem:start(?MODULE, Args, Opts).

start_0001_test() ->
    {ok, Pid} = start(),
    ?assertEqual(true, is_process_alive(Pid)),
    ?assertEqual(ok, stop(Pid)).
start_1001_test() ->
    {ok, Pid} = start([]),
    ?assertEqual(true, is_process_alive(Pid)),
    ?assertEqual(ok, stop(Pid)).
start_2001_test() ->
    {ok, Pid} = start([], []),
    ?assertEqual(true, is_process_alive(Pid)),
    ?assertEqual(ok, stop(Pid)).

%%--------------------------------------------------------------------
%% @doc wrappers around generic start_link functions.
%%
%%      ``` {ok, Pid} = stream:start_link().
%%          {ok, Pid2} = stream:start_link(StreamArgs).
%%          {ok, Pid3} = stream:start_link(StreamArgs, FSM_Opts).
%%      ```
%% @end
%%--------------------------------------------------------------------
-spec start_link() 
		-> {ok, pid()}.
start_link() ->
    start_link([], []).

-spec start_link( Args :: list()) 
		-> {ok, pid()}.
start_link(Args) ->
    start_link(Args, []).

-spec start_link( Args :: list()
		, Opts :: list()) 
		-> {ok, pid()}.
start_link(Args, Opts) ->
    gen_statem:start_link(?MODULE, Args, Opts).

start_link_0001_test() ->
    {ok, Pid} = start_link(),
    ?assertEqual(true, is_process_alive(Pid)),
    ?assertEqual(ok, stop(Pid)).
start_link_1001_test() ->
    {ok, Pid} = start_link([]),
    ?assertEqual(true, is_process_alive(Pid)),
    ?assertEqual(ok, stop(Pid)).
start_link_2001_test() ->
    {ok, Pid} = start_link([], []),
    ?assertEqual(true, is_process_alive(Pid)),
    ?assertEqual(ok, stop(Pid)).

%%--------------------------------------------------------------------
%% @doc wrappers around genenric stop function.
%%
%%      ``` ok = stream:stop(Pid).
%%      ```
%% @end
%%--------------------------------------------------------------------
stop(Pid) ->
    gen_statem:stop(Pid).

%%--------------------------------------------------------------------
%% @doc we use state functions as callback mode. Each fsm state
%%      is currently linked on the default pattern size extracted
%%      from buffer (bit, byte and custom).
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> atom().
callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%% @doc standard gen_statem otp initialization.
%% @end
%%--------------------------------------------------------------------
-spec init( Args :: list()) 
	  -> {ok, atom(), #state{}}.
init(Args) ->
    Mode = proplists:get_value(mode, Args, bit),
    mode(Mode).

%%--------------------------------------------------------------------
%% @doc initial mode switching, return a tuple.
%% @end
%%--------------------------------------------------------------------
-spec mode( Mode :: atom() | {customer, non_neg_integer()}) 
	  -> {ok, atom(), #state{}}.
mode(byte) ->
    {ok, bit, #state{ mode = byte, size = 8}};
mode({custom, Size}) ->
    {ok, custom, #state{ mode = custom, size = Size}};
mode(_) ->
    {ok, bit, #state{ mode = bit, size = 1 }}.

%%--------------------------------------------------------------------
%% @doc code_change callback. Currently not supported.
%% @end
%%--------------------------------------------------------------------
code_change(_,_,_,_) ->
    ok.

%%--------------------------------------------------------------------
%% @doc terminate callback. Currently not supported.
%% @end
%%--------------------------------------------------------------------
terminate(_,_,_) ->
    ok.

%%--------------------------------------------------------------------
%% @doc bit state is the main state of stream process. 
%% @end
%%--------------------------------------------------------------------
% insert data at the end of the buffer
bit(cast, {input, Bitstring}, Data) ->
    handle_cast({input, Bitstring}, Data);
% create a new process based on current buffer
bit({call, _From}, split, Data) ->
    {keep_state, Data};
% create a new process, based on current buffer with a shift
bit({call, _From}, {split, _Shift}, Data) ->
    {keep_state, Data};
% apply a function to one element of buffer
bit({call, From}, {map, Size, Function}, Data) ->
    lmap(From, Data, Size, Function);
% apply a function to one element of buffer with a shifting
bit({call, From}, {map, Size, Shift, Function}, Data) ->
    lmap(From, Data, Size, Shift, Function);
% copy a part of the buffer 
bit({call, From}, {copy, Size}, Data) ->
    lcopy(From, Data, Size);
% copy a part of the buffer with a shift
bit({call, From}, {copy, Size, Shift}, Data) ->
    lcopy(From, Data, Size, Shift);
% cut a part of the buffer (altering it)
bit({call, From}, {cut, Size}, Data ) ->
    lcut(From, Data, Size);
% cut a part of the buffer with a shift (altering it)
bit({call, From}, {cut, Size, Shift}, Data) ->
    lcut(From, Data, Size, Shift);

bit(cast, {mode, bit}, Data) ->
    {keep_state, Data};
bit(cast, {mode, byte}, Data) ->
    {next_state, byte, Data#state{ size = 8 }};
bit(cast, {mode, custom, Size}, Data) ->
    {next_state, custom, Data#state{ size = Size }};
bit(_Method, _Event, Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc byte state, get 8 bits long pattern from buffer.
%% @end
%%--------------------------------------------------------------------
byte(cast, {mode, bit}, Data) ->
    {next_state, bit, Data#state{ size = 1 }};
byte(cast, {mode, byte}, Data) ->
    {keep_state, Data};
byte(cast, {mode, custom, Size}, Data) ->
    {next_state, custom, Data#state{ size = Size }};
byte(cast, Event, Data) ->
    handle_cast(Event, Data);
byte(_Method, _Event, Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc custom state, set your own pattern size.
%% @end
%%--------------------------------------------------------------------
custom(cast, {mode, bit}, Data) ->
    {next_state, bit, Data#state{ size = 1 }};
custom(cast, {mode, byte}, Data) ->
    {next_state, custom, Data#state{ size = 8 }};
custom(cast, {mode, custom, Size}, Data) ->
    {keep_state, Data#state{ size = Size }};
custom(cast, Event, Data) ->
    handle_cast(Event, Data);
custom(_Method, _Event, Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc common cast shared across states.
%% @end
%%--------------------------------------------------------------------
% if we don't have enough data, insufficient is set and 
% contain the function and the caller. We can update our stream on
% demand and check when its good.
handle_cast( {input, Stream}
	   , #state{ insufficient = Resource } = Data) 
  when Resource =/= undefined ->
    {_, Data2} = linput(Data, Stream),
    case Resource of
	{From, {cut, Size}} -> lcut(From, Data2, Size);
	{From, {cut, Size, Shift}} -> lcut(From, Data2, Size, Shift);
	{From, {copy, Size}} -> lcopy(From, Data2, Size);
	{From, {copy, Size, Shift}} -> lcopy(From, Data2, Size, Shift);
	{From, {map, Size, Fun}} -> lmap(From, Data2, Size, Fun)
    end;
% standard input
handle_cast({input, Stream}, Data) ->
    linput(Data, Stream);
handle_cast({input, Stream, _Opts}, Data) ->
    linput(Data, Stream);
handle_cast(_Event, Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc common call shared across states.
%% @end
%%--------------------------------------------------------------------
handle_call(From, _Event, Data) ->
    {keep_state, Data, [{reply, From, ok}]}.

%%--------------------------------------------------------------------
%% @doc common info shared across states.
%% @end
%%--------------------------------------------------------------------
handle_info(_Event, Data) ->
    {keep_state, Data}.

%%====================================================================
%% Internal function
%%====================================================================

%%--------------------------------------------------------------------
%% @doc lerror function return standard error reply when something
%%      goes wrong.
%% @end
%%--------------------------------------------------------------------
-type error_reply() :: [{reply, term(), {error, term()}}].
-type lerror() :: {keep_state, #state{}, error_reply()}.
-spec lerror( Data :: #state{}
	    , From :: term()
	    , Reason :: term())
	    -> Reply :: lerror().
lerror(Data, From, Reason) ->
    { keep_state
    , Data
    , [{reply, From, {error, Reason}}]
    }.

%%--------------------------------------------------------------------
%% @doc lreply is a function helper to generate gen_statem reply
%%      data structure, containing callee and infinite stream
%%      as tuple.
%% @end
%%--------------------------------------------------------------------
-type ok_reply() :: {bitstring(), function()}.
-type lreply() :: [{reply, term(), ok_reply()}].
-spec lreply( From :: term()
	    , Head :: bitstring()
	    , Function :: atom()
	    , Args :: list()) 
	    -> Reply :: lreply().
lreply(From, Head, Function, Args) ->
    Reply = {Head, continuator(self(), Function, Args)},
    [{reply, From, Reply}].


%%--------------------------------------------------------------------
%% @doc linput got external bitstring and put it at the end of 
%%      buffer, in standard case, data of current state stored
%%      as record.
%% @end
%%--------------------------------------------------------------------
-spec linput( Data :: #state{}
	    , Input :: bitstring()) 
	    -> {keep_state, #state{}}.
linput(#state{ buffer = Buffer } = Data, Input) ->
    { keep_state
    , Data#state{ buffer = <<Buffer/bitstring, Input/bitstring>> }
    }.

%%--------------------------------------------------------------------
%% @doc lcut cut the first buffer part, altering data structure,
%%      and reply it to caller.
%% @end
%%--------------------------------------------------------------------
-spec lcut( From :: term()
	  , Data :: #state{}
	  , Size :: non_neg_integer()) 
	  -> Reply :: lreply() |
		      lerror().
lcut(From, #state{ buffer = Buffer } = Data, Size) ->
    try <<Pattern:Size/bitstring, Rest/bitstring>> = Buffer,
	 { keep_state
	 , Data#state{ buffer = Rest
		     , insufficient = undefined }
	 , lreply(From, Pattern, cut, [Size])
	 }
    catch 
	error:{badmatch, _Reason} -> 
	    { keep_state
	    , Data#state{ insufficient = {From, {cut, Size} } } 
	    }
    end.

%%--------------------------------------------------------------------
%% @doc same function, with a shifting.
%% @end
%%--------------------------------------------------------------------
-spec lcut( From :: term() 
	  , Data :: #state{}
	  , Size :: non_neg_integer()
	  , Shift :: non_neg_integer()
	  ) -> Reply :: lreply() |
		      lerror().
lcut(From, #state{ buffer = Buffer } = Data, Size, Shift) ->
    try <<Head:Shift/bitstring, Pattern:Size/bitstring, Rest/bitstring>> = Buffer,
	 { keep_state
	 , Data#state{ buffer = <<Head/bitstring, Rest/bitstring>> 
		     , insufficient = undefined }
	 , lreply(From, Pattern, cut, [Size, Shift])
	 }
    catch 
	error:{badmatch, _Reason} -> 
	    { keep_state
	    , Data#state{ insufficient = {From, {cut, Size, Shift} } } 
	    }
    end.

%%--------------------------------------------------------------------
%% @doc lcopy copy a part of the current stream without altering
%%      it and reply it to the caller.
%% @end
%%--------------------------------------------------------------------
-spec lcopy( From :: term()
	   , Data :: #state{}
	   , Size :: non_neg_integer()) 
	   -> Reply :: lreply() |
		       lerror().
lcopy(From, Data, Size) ->
    lcopy(From, Data, Size, 0).

%%--------------------------------------------------------------------
%% @doc same function, with a shifting.
%% @end
%%--------------------------------------------------------------------
-spec lcopy( From :: term() 
	   , Data :: #state{}
	   , Size :: non_neg_integer()
	   , Shift :: non_neg_integer()) 
	   -> Reply :: lreply() |
		       lerror().
lcopy(From, #state{ buffer = Buffer } = Data, Size, Shift) ->
    try <<_:Shift/bitstring, Head:Size/bitstring, _/bitstring>> = Buffer,
	 { keep_state
	 , Data
	 , lreply(From, Head, copy, [Size, Shift+Size])
	 }
    catch 
	error:{badmatch, Reason} -> lerror(Data, From, Reason)
    end.

%%--------------------------------------------------------------------
%% @doc lmap copy a part of the stream and apply a function on it
%%      without altering the state.
%% @end
%%--------------------------------------------------------------------
-spec lmap( From :: pid()
	  , Data :: #state{}
	  , Size :: non_neg_integer()
	  , Fun :: function())
	  -> Reply :: lreply() |
		      lerror().
lmap(From, Data, Size, Fun) ->
    lmap(From, Data, Size, 0, Fun).

%%--------------------------------------------------------------------
%% @doc lmap copy a part of the stream and apply a function on it
%%      without altering the state.
%% @end
%%--------------------------------------------------------------------
-spec lmap( From :: pid()
	  , Data :: #state{}
	  , Size :: non_neg_integer()
	  , Shift :: non_neg_integer()
	  , Fun :: function())
	  -> Reply :: lreply() |
		      lerror().
lmap(From, #state{ buffer = Buffer } = Data, Size, Shift, Fun) ->
    try <<_:Shift, Head:Size/bitstring, _/bitstring>> = Buffer,
	 { keep_state
	 , Data
	 , lreply(From, Fun(Head), map, [Size, Shift+Size, Fun])
	 }
    catch
	error:{badmatch,Reason} -> lerror(Data, From, Reason)
    end.


%%--------------------------------------------------------------------
%% @doc continuator function is an helper function to generate
%%      custom lambda function with defined value on it.
%% @end
%%--------------------------------------------------------------------
-spec continuator( Pid :: pid()
		 , Function :: atom()
		 , List :: list()) 
		 -> function().
continuator(Pid, map, [Size, Shift, Fun]) ->
    fun() -> map(Pid, Size, Shift, Fun)
    end;
continuator(Pid, cut, [Size]) ->
    fun() -> cut(Pid, Size)
    end;
continuator(Pid, cut, [Size, Shift]) ->
    fun() -> cut(Pid, Size, Shift)
    end;	   
continuator(Pid, copy, [Size]) ->
    fun() -> copy(Pid, Size)
    end;
continuator(Pid, copy, [Size, Shift]) ->
    fun() -> copy(Pid, Size, Shift+8)
    end.

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc local wrapper around gen_statem:call function
%% @end
%%--------------------------------------------------------------------
call(Pid, Event) ->
    call(Pid, Event, infinity).
call(Pid, Event, Timeout) ->
    gen_statem:call(Pid, Event, Timeout).

%%--------------------------------------------------------------------
%% @doc local wrapper around gen_statem:cast function
%% @end
%%--------------------------------------------------------------------
cast(Pid, Event) ->
    gen_statem:cast(Pid, Event).

%%--------------------------------------------------------------------
%% @doc local wrapper around gen_statem:info function
%% @end
%%--------------------------------------------------------------------
info(Pid, Event) ->
    gen_statem:cast(Pid, Event).

%%--------------------------------------------------------------------
%% @doc input is a cast and will always return ok except if process
%%      isn't present.
%%
%%      ``` ok = stream:input(Pid, <<"mystream">>.
%%          ok = stream:input(Pid, <<"mystream2">>.
%%      ```
%%
%% @end
%%--------------------------------------------------------------------
-spec input( Pid :: pid()
	   , Stream :: bitstring()) 
	   -> ok.
input(Pid, Stream) ->
    cast(Pid, {input, Stream}).

input_0001_test() ->
    {ok, Pid} = start_link(),
    input(Pid, <<"test">>),
    {bit, #state{ buffer = Buffer }} = sys:get_state(Pid),
    ?assertEqual(<<"test">>, Buffer).
input_0002_test() ->
    {ok, Pid} = start_link(),
    input(Pid, <<"test">>),
    input(Pid, <<"1234">>),
    {bit, #state{ buffer = Buffer }} = sys:get_state(Pid),
    ?assertEqual(<<"test1234">>, Buffer),
    ok = stop(Pid).

-spec input( Pid :: pid()
	   , Stream :: bitstring()
	   , Opts :: term()
	   ) -> ok.
input(Pid, Stream, _Opts) ->
    input(Pid, Stream).

input_1001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc cut alter current buffer stored in infinite stream FSM. This
%%      function will return current cutted pattern from buffer to
%%      the caller. We can do it based on first value or with
%%      defined shift.
%%
%%      ``` {Stream, Next} = stream:cut(Pid, 8).
%%          {Stream2, Next2} = Next().
%%      ```
%% @end
%%--------------------------------------------------------------------
-spec cut( Pid :: pid()
	 , Size :: non_neg_integer()) 
	 -> {bitstring(), function()}.
cut(Pid, Size) ->
    call(Pid, {cut, Size}).
cut_0001_test() ->
    {ok, Pid} = start_link(),
    input(Pid, <<"test">>),
    {T, _Fun} = cut(Pid, 8),
    ?assertEqual(<<"t">>, T),
    {bit, #state{ buffer = Buffer }} = sys:get_state(Pid),
    ?assertEqual(<<"est">>, Buffer),
    ok = stop(Pid).
cut_0002_test() ->
    {ok, Pid} = start_link(),
    % initialize bitstring
    input(Pid, <<"test">>),
    % first cut see cut_0001_test function
    {_T, Fun} = cut(Pid, 8),
    % second cut
    {T2, Fun2} = Fun(),
    {bit, #state{ buffer = Buffer }} = sys:get_state(Pid),
    ?assertEqual(<<"st">>, Buffer),
    ?assertEqual(<<"e">>, T2),
    % second cut
    {T3, Fun3} = Fun2(),
    {bit, #state{ buffer = Buffer2 }} = sys:get_state(Pid),
    ?assertEqual(<<"t">>, Buffer2),
    ?assertEqual(<<"s">>, T3),
    % third cut
    {T4, _Fun4} = Fun3(),
    {bit, #state{ buffer = Buffer3 }} = sys:get_state(Pid),
    ?assertEqual(<<>>, Buffer3),
    ?assertEqual(<<"t">>, T4),
    ok = stop(Pid).
cut_0003_test() ->
    {ok, Pid} = start_link(),
    timer(Pid, 10, <<"a">>),
    {T, _Fun} = cut(Pid, 4*8),
    ?assertEqual(<<"aaaa">>, T).

-spec cut( Pid :: pid()
	 , Size :: non_neg_integer()
	 , Shift :: non_neg_integer()) 
	 -> {bitstring(), function()}.
cut(Pid, Size, Shift) ->
    call(Pid, {cut, Size, Shift}).

cut_1001_test() ->
    {ok, Pid} = start_link(),
    input(Pid, <<"test">>),
    {T, _Fun} = cut(Pid, 8, 8),
    ?assertEqual(<<"e">>, T),
    {bit, #state{ buffer = Buffer }} = sys:get_state(Pid),
    ?assertEqual(<<"tst">>, Buffer),
    ok = stop(Pid).    
cut_1003_test() ->
    {ok, Pid} = start_link(),
    timer(Pid, 10, <<"a">>),
    {T, _Fun} = cut(Pid, 3*8, 8),
    ?assertEqual(<<"aaa">>, T),
    ok = stop(Pid).

-spec cut( Pid :: pid()
	 , Size :: non_neg_integer()
	 , Shift :: non_neg_integer()
	 , Opts :: term()
	 ) -> {bitstring(), function()}.
cut(Pid, Size, Shift, _Opts) ->
    cut(Pid, Size, Shift).

cut_2001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc copy retrieve a copied value of the current buffer managed
%%      by infinite stream FSM. Next value is based on the Size*Shift,
%%      where Size is the size of the pattern and Shift corresponding
%%      to the offset. When lambda function returned is executed you
%%      will automatically add shifted value.
%%
%%      ``` {Stream, Next} = stream:copy(Pid, 8).
%%          {Stream2, Next2} = Next().
%%      ```
%%      
%% @end
%%--------------------------------------------------------------------
-spec copy( Pid :: pid()
	  , Size :: non_neg_integer()) 
	 -> {bitstring(), function()}.
copy(Pid, Size) ->
    call(Pid, {copy, Size}).

copy_0001_test() ->
    {ok, Pid} = start(),
    ok = input(Pid, <<"thisisatest">>),
    {Out, _Fun} = copy(Pid, 8),
    ?assertEqual(<<"t">>, Out),
    stop(Pid).

-spec copy( Pid :: pid()
	 , Size :: non_neg_integer()
	 , Shift :: non_neg_integer()) 
	 -> {bitstring(), function()}.
copy(Pid, Size, Shift) ->
    call(Pid, {copy, Size, Shift}).

copy_1001_test() ->
    {ok, Pid} = start(),
    ok = input(Pid, <<"thisisatest">>),
    {Out, _Fun} = copy(Pid, 8, 0),
    ?assertEqual(<<"t">>, Out),
    stop(Pid).

-spec copy( Pid :: pid()
	  , Size :: non_neg_integer()
	  , Shift :: non_neg_integer()
	  , Opts :: term() 
	  ) -> {bitstring(), function()}.
copy(Pid, Size, Shift, _Opts) ->
    call(Pid, Size, Shift).

copy_2001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc map function will copy a value from current buffer and apply
%%      a function on this value (returning it to caller). This
%%      function doesn't alter the current buffer.
%%
%%      ``` Fun = fun(Pattern) -> {ok, Pattern}.
%%          {ModifiedPattern, Next} = stream:map(Pid, 8, Fun).
%%          {ModifiedPattern2, Next2} = Next().
%%      ```
%% @end
%%--------------------------------------------------------------------
-spec map( Pid :: pid()
	 , Size :: non_neg_integer()
	 , Fun :: function()) 
	 -> {bitstring(), function()}.
map(Pid, Size, Fun) ->
    call(Pid, {map, Size, Fun}).

map_0001_test() ->
    {ok, Pid} = start(),
    ok = input(Pid, <<"thisisatest">>),
    {Term, _Fun} = map(Pid, 8, fun(A) -> {A, true} end),
    ?assertEqual({<<"t">>, true}, Term),
    ok = stop(Pid).
map_0002_test() ->
    {ok, Pid} = start(),
    ok = input(Pid, <<"thisisatest">>),
    {Term, Fun} = map(Pid, 8, fun(A) -> {A, true} end),
    ?assertEqual({<<"t">>, true}, Term),
    {Term2, _Fun2} = Fun(),
    ?assertEqual({<<"h">>, true}, Term2),
    ok = stop(Pid).

-spec map( Pid :: pid()
	 , Size :: non_neg_integer()
	 , Shift :: non_neg_integer()
	 , Fun :: function())
	 -> {bitstring(), function()}.
map(Pid, Size, Shift, Fun) ->
    call(Pid, {map, Size, Shift, Fun}).

map_1001_test() ->
    {ok, Pid} = start(),
    ok = input(Pid, <<"thisisatest">>),
    {Term, _Fun} = map(Pid, 8, 0, fun(A) -> {A, true} end),
    ?assertEqual({<<"t">>, true}, Term),
    ok = stop(Pid).

-spec map( Pid :: pid()
	 , Size :: non_neg_integer()
	 , Shift :: non_neg_integer()
	 , Fun :: function()
	 , Opts :: term() ) 
	 -> {bitstring(), function()}.
map(Pid, Size, Shift, Fun, _Opts) ->
    call(Pid, {map, Size, Shift, Fun}).

map_2001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc Run a timer, sending stream at specific time interval.
%%      This function require timer application and is mainly
%%      used for testing purpose.
%%
%%      ``` {ok, Ref} = stream:timer(Pid, 1000, <<"mydata">>).
%%      ```
%% @end
%%--------------------------------------------------------------------
-spec timer( Pid :: pid()
	   , Time :: non_neg_integer()
	   , Stream :: bitstring()) 
	   -> {ok, {interval, reference()}}.
timer(Pid, Time, Bitstring) ->
    timer:apply_interval(Time, stream, input, [Pid, Bitstring]).

timer_0001_test() ->
    ok.

-spec timer( Pid :: pid()
	   , Time :: non_neg_integer()
	   , Stream :: bitstring()
	   , Opts :: term()
	   ) -> {ok, {interval, reference()}}.
timer(Pid, Time, Bitstring, _Opts) ->
    timer(Pid, Time, Bitstring).

timer_1001_test() ->
    ok.
