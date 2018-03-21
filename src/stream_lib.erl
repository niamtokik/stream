%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2017, Mathieu Kerjouan
%%% @doc = Standard Interface Definition and Messages =
%%%
%%%      == Input/Output Message ==
%%%
%%%      These messages are used to import/extract stream buffer.
%%%
%%%        - { input, bitstring() }
%%%        - { output, pid(), bitstring() }
%%%
%%%      == Internal Message ==
%%%
%%%      Internal message used by our stream FSM.
%%%
%%%        - { Reference, insufficient, Replay}
%%%
%%%      == Buffer Interaction ==
%%%
%%%      Common interface to cut, copy, and map buffer. These
%%%      function are usually used with our API.
%%%
%%%        - { cut, {Size, Shift, Options} }
%%%        - { copy, {Size, Shift, Options} }
%%%        - { map, {Size, Shift, Function, Options} }
%%%
%%%      == Automatic Action on Buffer ==
%%%
%%%      These messages are mainly used to act automatically
%%%      on some patterns and alter our FSM state.
%%%
%%%        - { action, {Size, Pattern, Function, Options} }
%%%        - { search, {Size, Pattern, Function, Options} }
%%%        - { parse, {Module, Function, Args, Options} }
%%%
%%%      == Behavior Configuration ==
%%%
%%%      These messages are used to alter current FSM state
%%%      variables.
%%%
%%%        - { set, mode, Size }
%%%        - { set, limit, Size}
%%%        - { set, marks }
%%%
%%%      == Spread (automatic and manual) ==
%%%
%%%      These messages will permit to make static or dynamic
%%%      copy of the stream on other stream. The main stream
%%%      is kept intact, but, based on some marks on it,
%%%      forwarded to another copy of the FSM (with different
%%%      behavior).
%%%
%%%        - wip.
%%% 
%%%      = stream_lib =
%%%
%%%      This file extract all internal functions from stream
%%%      gen_statem. This make thing easier to test and standard
%%%      to all other gen_stream implementation.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stream_lib).
-export([input/2]).
-export([cut/3]).
-export([copy/3]).
%-export([map/3, map/4]).
%-export([filter/3, filter/4]).
%-export([action/3, action/4]).
%-export([search/3, search/4]).
%-export([parse/3, parse/4]).
%-export([continuator/3, continuator/4]).
-compile(export_all).
-include("stream.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec input( Data :: #state{}
	   , Input :: bitstring()
	   ) -> {keep_state, #state{}}.		   
input(#state{ buffer = Buffer } = Data, Input) ->
    { keep_state
    , Data#state{ buffer = <<Buffer/bitstring, Input/bitstring>> }
    };
input(_Data, Input) -> 
    {stop, {badinput, Input}}.

input_0000_test() ->
    IN = <<"thisisatest">>,
    OUT = {keep_state, #state{ buffer = <<"thisisatest">> }},
    ?assertEqual(OUT, input(#state{}, IN)).
input_0001_test() ->
    Data = #state{ buffer = <<"abcd">> },
    IN = <<"thisisatest">>,
    OUT = {keep_state, #state{ buffer = <<"abcdthisisatest">> }},
    ?assertEqual(OUT, input(Data, IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
cut(Data, From, {Size, Shift}) ->
    cut(Data, From, {Size, Shift, []});
cut(#state{ mode = Mode, buffer = Buffer} = Data
   ,From
   ,{Size, Shift, Opts}) ->
    ShiftSize = Shift*Mode,
    PatternSize = Size*Mode,
    try <<Head:ShiftSize/bitstring, 
	  Pattern:PatternSize/bitstring, 
	  Rest/bitstring>> = Buffer,
	 { keep_state
	 , Data#state{ buffer = <<Head/bitstring, Rest/bitstring>> }
	 , [{reply, From, {Pattern, function}}] }
    catch
	error:{badmatch,_Reason} ->
	    Insufficient = {From, {cut, Size, Shift, Opts} },
	    { keep_state, Data#state{ insufficient = Insufficient } }
    end.

cut_0000_test() ->
    Data = #state{ buffer = <<"abcdefghijkl">> },
    IN = cut(Data, from, {8, 0}),
    OUT = { keep_state
	  , Data#state{ buffer = <<"bcdefghijkl">> }
	  , [{reply, from, {<<"a">>, function}}] 
	  },
    ?assertEqual(OUT, IN).
cut_0001_test() ->
    Data = #state{ buffer = <<"a">> },
    IN = cut(Data, from, {16,0}),
    OUT = { keep_state
	  , Data#state{ insufficient = {from, {cut, 16, 0, []}} }
	  },
    ?assertEqual(OUT, IN).
cut_0002_test() ->
    Data = #state{ buffer = <<"abc">> },
    IN = cut(Data, from, {8, 8}),
    OUT = { keep_state
	  , Data#state{ buffer = <<"ac">> }
	  , [{reply, from, {<<"b">>, function}}]
	  },
    ?assertEqual(OUT, IN).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec copy( Data :: #state{}
	  , From :: pid()
	  , { Size :: non_neg_integer()
	    , Shift :: non_neg_integer()
	    , Opts :: term() }
	  ) -> {keep_state, #state{}}.
copy(Data, From, {Size, Shift}) -> 
    copy(Data, From, {Size, Shift, []});
copy(#state{ mode = Mode, buffer = Buffer } = Data
    ,From
    ,{Size, Shift, Opts}) ->
    ShiftSize = Shift*Mode,
    PatternSize = Size*Mode,
    try <<_:ShiftSize/bitstring, 
	  Pattern:PatternSize/bitstring, 
	  _/bitstring>> = Buffer,
	 { keep_state, Data
	 , [{reply, From, {Pattern, function}}] }
    catch
	error:{badmatch,_Reason} ->
	    Insufficient = {From, {copy, Size, Shift, Opts} },
	    { keep_state, Data#state{ insufficient = Insufficient } }
    end.

copy_0000_test() ->
    Data = #state{ buffer = <<"abcdefghij">> },
    IN = copy(Data, from, {8,0}),
    OUT = { keep_state, Data
	  , [{reply, from, {<<"a">>, function}}] 
	  },
    ?assertEqual(OUT, IN).
copy_0001_test() ->
    Data = #state{ buffer = <<"a">> },
    IN = copy(Data, from, {16,0}),
    OUT = { keep_state
	  , Data#state{ insufficient = {from, {copy, 16, 0, []}} }
	  },
    ?assertEqual(OUT, IN).
copy_0002_test() ->
    Data = #state{ buffer = <<"abc">> },
    IN = copy(Data, from, {8,8}),
    OUT = { keep_state, Data
	  , [{reply, from, {<<"b">>, function}}]
	  },
    ?assertEqual(OUT, IN).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec map( Data :: #state{}
	 , From :: pid()
	 , { Size :: non_neg_integer()
	   , Shift :: non_neg_integer()
	   , Function :: function()
	   , Opts :: term() }
	 ) -> { keep_state
	      , #state{}
	      , [{reply, pid(), {term(), function()}}]}.

map(Data, From, {Size, Shift, Function}) ->
    map(Data, From, {Size, Shift, Function, [{mode, cut}]});
map(Data, From, {Size, Shift, Function, Opts}) ->
    Mode = proplists:get_value(mode, Opts, cut),
    map(Mode, Data, From, {Size, Shift, Function, Opts}).

map_0000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec map( Mode :: copy | cut 
	 , Data :: #state{}
	 , From :: pid()
	 , { Size :: non_neg_integer()
	   , Shift :: non_neg_integer()
	   , Function :: function() }
	 ) -> { keep_state
	      , #state{}
	      , [{reply, pid(), {term(), function()}}]}.

map(cut, Data, From, {Size, Shift, Function}) ->
    map(cut, Data, From, {Size, Shift, Function, []});
map(cut, Data, From, {Size, Shift, Function, Opts}) ->
    ok;
map(copy, Data, From, {Size, Shift, Function}) ->
    map(copy, Data, From, {Size, Shift, Function, []});
map(copy, Data, From, {Size, Shift, Function, Opts}) ->
    ok.

map_cut_0000_test() ->
    ok.

map_copy_0000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter( Data :: #state{}
	    , From :: pid()
	    , { Size :: non_neg_integer()
	      , Shift :: non_neg_integer()
	      , Pattern :: bitstring() | function() }
	    ) -> { keep_state
		 , #state{} 
		 , Reference :: term() }.
filter(Data, From, {Size, Shift, Pattern}) -> 
    ok.

filter_0000_test() ->
    ok.

-spec filter( Data :: #state{}
	    , From :: pid()
	    , { Size :: non_neg_integer()
	      , Shift :: non_neg_integer()
	      , Pattern :: bitstring() | function() }
	    , Opts :: term()
	    ) -> {keep_state, #state{}}.
filter(Data, From, {Size, Shift, Pattern}, Opts) ->
    ok.

filter_1000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec action( Data :: #state{}
	    , From :: pid()
	    , { Size :: non_neg_integer()
	      , Shift :: non_neg_integer()
	      , Pattern :: bitstring()
	      , Function :: function() 
	      , Opts :: term() }
	    ) -> {keep_state
		 , #state{}
		 , [{reply, pid(), {pid(), Reference ::term()}}]
		 }.
action(Data, From, {Size, Shift, Pattern, Function}) ->
    ok;
action(Data, From, {Size, Shift, Pattern, Function, Opts}) -> 
    ok.

action_0000_test() ->
    ok.

action(cut, Data, From, {Size, Shift, Pattern, Function}) ->
    ok;
action(copy, Data, From, {Size, Shift, Pattern, Function}) ->
    ok.

action_cut_0000_test() ->
    ok.

action_copy_0000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
search(Data, From, {Size, Shift, Pattern}) -> 
    ok.

search_0000_test() ->
    ok.

search(Data, From, {Size, Shift, Pattern}, Opts) ->
    ok.

search_1000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse(Data, From, {Module, Function}) ->
    ok.

parse_0000_test() ->
    ok.

parse(Data, From, {Module, Function}, Opts) ->
    ok.

parse_1000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
% stream:Function({Size, Shift, Arguments, Opts}).
continuator(#state{ mode = Mode }, Pid, map, {Size, Shift, Function}) -> 
    fun() -> 
	    stream:map(Pid, Size, Shift, Function)
    end;
continuator(#state{ mode = Mode }, Pid, map, {Size, Shift, Function, Opts}) -> 
    fun(Args) ->
	    stream:map(Pid, Size, Shift, Function, [Args|Opts])
    end;
continuator(Data, Pid, cut, {Size, Shift}) -> 
    fun() ->
	    stream:cut(Pid, Size, Shift)
    end;
continuator(Data, Pid, cut, {Size, Shift, Opts}) -> 
    fun(Args) ->
	    stream:cut(Pid, Size, Shift, [Args|Opts])
    end;
continuator(Data, Pid, copy, {Size, Shift}) -> 
    fun() ->
	    stream:copy(Pid, Size, Shift)
    end;
continuator(Data, Pid, copy, {Size, Shift, Opts}) -> 
    fun(Args) ->
	    stream:copy(Pid, Size, Shift, [Args|Opts])
    end.
	    
    
	    
		  

