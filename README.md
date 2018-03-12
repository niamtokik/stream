# stream

`stream`  is   an  Erlang/OTP  library   implementing  infinite-stream
data-structure  based   on  bitstring.  This  library   give  you  the
possibility  to  cut,  copy,  act  and   react  on  any  kind  of  raw
bitstrings. This implementation is currently in active development and
should not be used in production.

## Build

    $ rebar3 compile

## Test

`stream` support unit testing with eunit:

    $ rebar3 eunit
	
You can also use common testing:

    $ rebar3 ct

## Documentation

`stream` documentation is managed with edoc:

    $ rebar3 edoc

Lot of code example are available from unit and common testing. Please
read them.

## Usage

`stream` use  edoc module, all  functions are self documented.  Here a
simple usage example of this library:

    > {ok, Stream} = stream:start.
	> stream:timer(Stream, 1000, crypto:rand_bytes(256).
	> stream:cut(Stream, 1024).

## Compatibility

 - [x] Erlang-19
 - [x] Erlang-20

## Hex

`stream` will be available soon on hex.pm.

## Backup

`stream` is available on different place on the web:

 * bitbucket: https://bitbucket.org/niamtokik/stream
 * gitlab: https://gitlab.com/niamtokik/stream
 
These repositories are not automatically synced.

## License

OpenBSD License 'aka' modified ISC License.
