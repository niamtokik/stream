%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2017, Mathieu Kerjouan
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------
-module(stream_lib).
-export([input/2, input/3]).
-export([cut/3, cut/4]).
-export([copy/3, copy/4]).
-export([map/3, map/4]).
-export([filter/3, filter/4]).
-export([action/3, action/4]).
-export([search/3, search/4]).
-export([parse/3, parse/4]).
-export([continuator/3, continuator/4]).
-include("stream.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec input( Data :: #state{}
	   , Bitstring :: bitstring()
	   ) -> {keep_state, #state{}}.		   
input(Data, Bitstring) -> 
    input(Data, Bitstring, []).

input_0000_test() ->
    IN = <<"thisisatest">>,
    OUT = {keep_state, #state{ buffer = <<"thisisatest">> }},
    ?assertEqual(OUT, input(#state{}, IN)).
input_0001_test() ->
    Data = #state{ buffer = <<"abcd">> },
    IN = <<"thisisatest">>,
    OUT = {keep_state, #state{ buffer = <<"abcdthisisatest">> }},
    ?assertEqual(OUT, input(Data, IN)).

-spec input( Data :: #state{}
	   , Bitstring :: bitstring()
	   , Opts :: list()
	   ) -> {keep_state, #state{}}.
input(#state{ buffer = Buffer } = Data, Input, _Opts) ->
    { keep_state
    , Data#state{ buffer = <<Buffer/bitstring, Input/bitstring>> }
    };
input(Data, Bitstring, Opts) -> ok.

input_1000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cut( Data :: #state{}
	 , From :: pid()
	 , { Size :: non_neg_integer()
	   , Shift :: non_neg_integer() }
	 ) -> {keep_state, #state{}}.		  
cut(Data, From, {Size, Shift}) -> 
    ok.

cut_0000_test() ->
    ok.

-spec cut( Data :: #state{}
	 , From :: pid()
	 , { Size :: non_neg_integer()
	   , Shift :: non_neg_integer() }
	 , Opts :: term()
	 ) -> {keep_state, #state{}}.
cut(Data, From, {Size, Shift}, Opts) ->
    ok.

cut_1000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec copy( Data :: #state{}
	  , From :: pid()
	  , { Size :: non_neg_integer()
	    , Shift :: non_neg_integer() }
	  ) -> {keep_state, #state{}}.
copy(Data, From, {Size, Shift}) -> 
    ok.

copy_0000_test() ->
    ok.

-spec copy( Data :: #state{}
	  , From :: pid()
	  , { Size :: non_neg_integer()
	    , Shift :: non_neg_integer() }
	  , Opts :: term()
	  ) -> {keep_state, #state{}}.
copy(Data, From, {Size, Shift}, Opts) ->
    ok.

copy_1000_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec map( Data :: #state{}
	 , From :: pid()
	 , { Size :: non_neg_integer()
	   , Shift :: non_neg_integer()
	   , Function :: function() }
	 ) -> {keep_state, #state{}}.
map(Data, From, {Size, Shift, Function}) -> 
    ok.

map_0000_test() ->
    ok.

-spec map( Data :: #state{}
	 , From :: pid()
	 , { Size :: non_neg_integer()
	   , Shift :: non_neg_integer()
	   , Function :: function() }
	 , Opts :: term()
	 ) -> {keep_state, #state{}}.
map(Data, From, {Size, Shift, Function}, Opts) ->
    ok.

map_1000_test() ->
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
	    ) -> {keep_state, #state{}}.
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
	      , Function :: function() }
	    ) -> {keep_state, #state{}}.
action(Data, From, {Size, Shift, Pattern, Function}) -> 
    ok.

action_0000_test() ->
    ok.

-spec action( Data :: #state{}
	    , From :: pid()
	    , { Size :: non_neg_integer()
	      , Shift :: non_neg_integer()
	      , Pattern :: bitstring()
	      , Function :: function() }
	    , Opts :: term()
	    ) -> {keep_state, #state{}}.
action(Data, From, {Size, Shift, Pattern, Function}, Opts) ->
    ok.

action_1000_test() ->
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
continuator(StreamID, Function, Arguments) -> 
    ok.

continuator_0000_test() ->
    ok.

continuator(StreamID, Function, Arguments, Opts) -> 
    ok.

continuator_1000_test() ->
    ok.
