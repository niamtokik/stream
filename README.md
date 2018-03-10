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

## Todo List

All these features should be added as tickets on github.

### Interfacing

 - [ ] make this gen_statem as behaviour template
 - [ ] Need to add optional parameters to each functions
 - [ ] make it as erlang behaviour
 - [ ] re-implement it with handle_event_function instead of state_function callback mode (more readability?)
 - [ ] re-implement it with gen_server behavior (more performance?)
 - [ ] using timeout instead of error message

### Functionality

 - [ ] Add filter function, to clean buffer
 - [ ] Add search function, to find a specific patterns
 - [ ] Add action function, will react when some kind of pattern arrive
 - [ ] Add "cut" or "copy", we should choose when we want to alter buffer (or not)
 - [ ] handle other kind of structure (list, map, custom one...)?
 - [ ] add automatic parsing feature based on FSM lambda function or module
 - [ ] Create standard/custom size
 - [ ] create an API to generate loop over stream automatically
 - [ ] Polling event automatically
 - [ ] add compression feature
 - [ ] add deduplication feature
 - [ ] add active mode (this behaviour connecting to something)
 - [ ] dynamic size (change based on data received)

### Reliability

 - [x] Handle error in different way, currently, we return an error when something goes wrong, we need a more intelligent return
 - [ ] add erlang code version to manage host update
 - [ ] benchmark testing from random bytes generator, local files and more
 - [ ] add pre/post callback with alerting
 - [x] Add more unit test
 - [ ] Create common test
 - [ ] add pre/post callback with alerting
 - [x] add more documentation and example usage
 - [x] add passive mode (receive from somewhere)
 - [ ] add debug information
 - [ ] find a way to handle error correctly based on bitstring size

### Scalability

 - [ ] shared stream (act as bucket without ordering, got data and just push it on buffer)
 - [ ] dedicated stream (process sharing common reference and access input only from each one)
 - [ ] Add bookmarks feature, multiple process could have its own bookmarks and navigate with it
 - [ ] Add callback manager (logging event & more)
 - [ ] Add Copy mode with splitted buffer (create a new process with buffer copied from original)
 - [ ] Copied mode updated dynamically from parent process
 - [ ] Add subscription feature, relaying data from buffer to another process or react to some events
 - [ ] Add different backend for buffer (will be a bit hard to use it, but could be cool)
 - [ ] Add buffer limits
 - [ ] Add forwarder size limit
 - [ ] Add QoS feature (rate limiting)
 - [ ] Add reference management, when a cut is made, we keep copy reference up to date with removed data, based on client state

### Other idea

 - [ ] Make different protocol interfaces (ftp, http, ssh...)
 - [ ] create a simple converter interface from ports/tcp/udp... -> input
 - [ ] Create a transactional datastructure when something is written on buffer

## Backup

`stream` will be available soon on gitlab and bitbucket as backup.

## License

OpenBSD License 'aka' modified ISC License.
