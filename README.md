# stream

`stream` is an Erlang/OTP library.

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

stream use edoc module, all functions are self documented. Here a
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

`stream` will be available soon on gitlab and bitbucket as backup.

## License

OpenBSD License 'aka' modified ISC License.
