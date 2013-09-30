This is a homepage of Piqi-RPC -- an RPC-over-HTTP system for Erlang.

Overview
--------

Piqi-RPC gives Erlang developers a convenient and reliable way to connect
services written in Erlang with non-Erlang clients using JSON, XML or Google
Protocol Buffers over HTTP.

Basically, Piqi-RPC can be viewed as an HTTP gateway for Erlang functions. Any
standard HTTP client can communicate with them not knowing anything about Erlang
or Piqi-RPC. And the opposite is also true: Erlang functions work with idiomatic
Erlang data structures and don't have to be aware of HTTP or even Piqi-RPC (note
that it is still possible to get details of HTTP request from application code).

In addition to calling Erlang functions using standard HTTP, Piqi-RPC also comes
with `piqi call` -- a native client for calling Piqi-RPC Erlang functions using
command-line interface. As in case of HTTP, Erlang implementation remains
completely oblivious about command-line interface and the particular method used
for its invocation.

In other words, Piqi-RPC system automates several areas where Erlang developers
usually need to write a lot of plumbing code:

**Parsing and validating input parameters**
:   Piqi-RPC automatically parses XML, JSON, Protocol Buffers and command-line
    arguments, validates them and converts to idiomatic Erlang data structures.

**Generating output parameters**
:   Again, Piqi-RPC automatically generates XML, JSON and Protocol Buffers from
    native Erlang representation.

**HTTP communication**
:   Piqi-RPC provides fully-compliant HTTP communication layer. It receives and
    validates HTTP requests, sends responses, handles "Content-Type" and
    "Accept" headers and generates around 10 different meaningful HTTP response
    types for various error conditions that can occur during request execution.

    Piqi-RPC also handles service registration and dispatching which allows many
    services to run concurrently using different HTTP URLs.

In the current Piqi-RPC implementation, services are stateless. That is Piqi-RPC
doesn't maintain state between calls and doesn't perform any request
synchronization. If one needs their service to have a state or prevent several
functions from being executed concurrently, he would need to implement such
functionality manually.

Piqi-RPC can be used for automating only Erlang servers -- currently, it doesn't
have support for writing clients.

One of Piqi-RPC fundamental features is the ability to make changes in protocol
while staying backward compatible with exiting clients and even servers. Such
changes may include adding a new parameter, turning a scalar parameter to a
list, or making a required parameter into an optional one.

Although Piqi-RPC currently supports only HTTP, other transports can be easily
added if needed. Core Piqi-RPC interfaces are transport-agnostic.

Piqi-RPC user guide
-------------------

General Piqi-RPC usage scenario for an Erlang service implementer works as
follows.

1. Build and install Piqi-RPC Erlang application and dependencies.

2. Describe service data and functions using the [Piqi](/doc/piqi/) data
definition language.

3. Call `piqic-erlang-rpc` Piqi compiler to generate serialization/
deserialization functions and server stubs for Erlang.

4. Implement server callback functions (i.e. the actual service
implementation).

5. Start the `piqi_rpc` Erlang application and register the new Piqi-RPC
service.

The following sections describe each step in detail.

### Getting Piqi-RPC

Piqi-RPC can be built and installed from source code or it can be installed as a
[rebar](https://github.com/rebar/rebar) package.

#### Piqi-RPC source code

Piqi-RPC source code is available in "piqi-rpc" directory of the Piqi GitHub
[repository](http://github.com/alavrik/piqi/tree/dev/piqi-rpc).

#### Piqi-RPC rebar package

Piqi-RPC rebar package is maintained in a separate GitHub
[repository](https://github.com/alavrik/piqi-rpc).

### Defining a Piqi-RPC service

Refer to the [Piqi](/doc/piqi/) documentation for information how define Piqi
functions and data types.

There is an example of service definition in the [Examples](#examples) section
below.

### Calling `piqic-erlang-rpc` Piqi compiler

`piqic-erlang-rpc` Piqi compiler should be used in exactly the same way as the
basic `piqic erlang` compiler which is documented [here](/doc/erlang/).

In addition to the output Erlang files produced by `piqic erlang`,
`piqic-erlang-rpc` generates two additional files: `<Mod>_piqi_rpc.erl` and
`<Mod>_piqi_impl.hrl`.

The first generated file contains Piqi-RPC server stubs.

The second file contains Erlang function specifications (i.e. `-spec ...`) for
the service implementation functions.

### Registering a Piqi-RPC service

Before registering a Piqi-RPC service, make sure that the `piqi_rpc` Erlang
application is running. It can be started by calling

    piqi_rpc:start().

Piqi-RPC service registration is performed by calling the
`piqi_rpc:add_service(<service-tuple>)` function, where `<service-tuple>` has
the following format:

    {ImplMod, RpcMod, UrlPath}

or

    {ImplMod, RpcMod, UrlPath, Options}

`ImplMod` contains implementation of the Piqi-RPC service defined by `RpcMod`.
Note that `ImplMod` can contain implementations of more than one Piqi-RPC
service.

`RpcMod` is a module generated by the `piqic-erlang-rpc` compiler from Piqi
specification. The module is usually named as `<Mod>_piqi_rpc`.

`UrlPath` is a base URL path of the HTTP resource for the Piqi-RPC service. For
example, if the URL path is set to `bar/foo`, the service functions can be
called using `http://<server>:<port>/bar/foo/<function-name>` URL.

`Options` is a list of service options, see the description of
`default_service_options` configuration entry below for details.

Examples:

    {foo_impl, foo_piqi_rpc, "bar/foo"}

    {foo_impl, foo_piqi_rpc, "bar/debug_foo", [
        {pretty_print, true},
        {use_strict_parsing, true}
    ]}

    {foo_impl, foo_piqi_rpc, "bar/production_foo", [
        {pretty_print, false},
        {omit_internal_error_details, true}
    ]}

### Unregistering a Piqi-RPC service

Piqi-RPC service can be unregistered by calling

    piqi_rpc:remove_service(<service-tuple>)

### Configuring Piqi-RPC services in Erlang `.config` files

`piqi_rpc` Erlang application has the following main configuration entries:

-   `rpc_services`

    This entry specifies a list of statically configured Piqi-RPC services. Each
    entry is represented as `<service-tuple>` which format is described in the
    previous sections.

-   `default_service_options`

    This entry defines default options for Piqi-RPC services. Default options
    defined here can be overridden for a specific service.

    Service options control various aspects of service behavior, such as

    -   pretty-print JSON and XML output (default = true)

    -   omit fields with 'null' values from JSON output (default = true)

    -   treat unknown and duplicate fields as errors when parsing JSON or XML
        input (default = false)

    -   do not include Erlang stracktraces in HTTP 500 responses when request
        handler crashes (default = true)

Full documentation for `piqi_rpc` Erlang application configuration is available
at
[piqi\_rpc.app.src](https://github.com/alavrik/piqi/blob/dev/piqi-rpc/src/piqi_rpc.app.src).

### Getting details of HTTP request from service implementation code

Piqi-RPC provides a way to access original HTTP request parameters from the
application code. This can be useful, for example, if some details of a request
or session are passed using HTTP headers.

Before calling a user's service implementation function, Piqi-RPC saves a
Webmachine's `wrq` record in the process dictionary so that it can be accessed
later using `erlang:get(wrq)`. Refer to the [Webmachine
documentation](http://webmachine.basho.com/reqdata.html) for information about
how to extract details of HTTP request from the `wrq` object.

### Modifying details of HTTP response from service implementation code

Similarly to getting details of HTTP request, the same mechanism is applicable
to modifying HTTP responses.

Application code can manipulate Webmachine's `wrq` data structure and save the
modified version version of it by calling `erlang:put(wrq)`.

This can be used, for example, to set custom HTTP response headers, return
cookies, etc.

### Making Piqi-PRC calls over HTTP

Piqi-RPC calls are performed as HTTP POST requests. Input parameters for a
remote function are included in the post body and can be represented as JSON,
XML or Protocol Buffers data structure. Output parameters are sent back in HTTP
response body encoded in one of the above formats.

The [Encodings](/doc/encodings/) section of the Piqi documentation describes the
rules how data is represented in JSON, XML and Protocol Buffers format.

#### `Content-Type` and `Accept` headers

Input and output format is controlled by standard HTTP `Content-Type` and
`Accept` headers.

Piqi-RPC supports the following media types: `application/json`,
`application/xml` and `application/x-protobuf`.

If `Accept` header is not specified in the request, the output will be formatted
as JSON.

#### Success response codes (2XX)

In case of successful function call, one of `200 "OK"` or `204 "No Content"`
status codes is returned.

The `204 "No Content"` status code is used only when the function doesn't have
output parameters.

#### Error response codes (4XX)

The following response codes can be generated by Piqi-RPC. In most cases, HTTP
response will contain a body with a plain-text description of the problem that
triggered the error response.

400 “Bad Request”
:   This status code is used when something is wrong with the request, for
    example, when input parameters do not conform to the interface definition.

404 “Not Found”
:   This status code is returned if a function is called what wasn't defined by
    the Piqi interface definition.

405 "Method not allowed"
:   This status code is returned when other HTTP request method than `POST` or
    `GET` is called.

406 "Not Acceptable"
:   This status code is used when the request's `Accept` header doesn't allow to
    choose one of the supported media types for the response.

415 “Unsupported media type”
:   This status code is returned when `Content-Type` header of the request is
    not one of the supported media types.

411 “Length required”
:   This status code is used when the body of the request is empty and the
    called function is expected to have non-emtpy input.

#### Error response codes (5XX)

500 "Internal Server Error"
:   This status code is used in two cases:

    1.  When an application error is returned by the implementation function. In
        such case, the error data structure will be encoded according to the
        `Accept` request header and the `Content-Type` header of the response
        will be set accordingly. This way the calling application will be able
        to interpret the returned application error and provide specific
        handling if necessary.

    2.  When something goes wrong during the request execution. For example,
        when the implementation function crashes. In such case, the description
        of the error will be returned in plain text and the `Content-Type`
        header is set to `text/plain`.

503 "Service Unavailable"
:   This status code is currently not used. In future, it will mean that the
    Piqi-RPC service is paused.

502 "Bad Gateway"
:   This status code is used when the implementation function returns a value
    that doesn't conform to the original Piqi interface definition.

### Getting Piqi specification from a running Piqi-RPC service

Piqi-RPC provides a method to get the Piqi interface definition from a running
service. This can be done by sending an HTTP `GET` request with any HTTP URL
that starts with the service base URL (the base URL is defined during service
registration).

For example, if a service is running at `http://localhost:8888/foo/bar` the Piqi
service definition can be retrieved by running any of the following HTTP
requests:

    curl 'http://localhost:8888/foo/bar'

    curl 'http://localhost:8888/foo/bar/have-no-idea-what-functions-are-there'

This functionality is used by Piqi-RPC command-line client described in the next
section. The command-line client can fetch Piqi specifications of remote
services and display them in several different formats.

### `piqi call` -- Piqi-RPC command-line client

`piqi call` interprets command-line arguments as input parameters for a remote
function, converts them into a Protobuf-encoded data object and executes a
Piqi-RPC function call over HTTP.

In addition to calling a remote function, `piqi call` can fetch Piqi
specifications of a remote service and print them in several formats: Piqi
(`--piqi` flag), Piqi-light (`-p` flag) and getopt-style help for remote
functions (`-h` flag).

See some `piqi call` usage scenarios in the [Examples](#examples) section below.

`piqi call` command is documented [here](/doc/tools/#piqicall).

### `piqi_rpc` Erlang application

By default, Piqi-RPC runs an HTTP server on port 8888 on all available network
interfaces.

These parameters as well as some others specific to Mochiweb HTTP server can be
customized through `piqi_rpc` application environment configuration (i.e.
`piqi_rpc.app` or Erlang `.config` file).

### Examples

There are several complete examples of Piqi-RPC servers and clients available in
the
["piqi-rpc/examples"](https://github.com/alavrik/piqi/tree/master/piqi-rpc/examples)
sub-directory.

One of the examples contains a simple addressbook service that includes three
functions:

-   add a person to the addressbook

-   get a person from the addressbook by identifier

-   list all addressbook entries

Below is complete definition of the service in [Piqi](/doc/piqi/) language. This
definition relies on another Piqi module named `person`. You can find its
definition in the source code, or on the [Examples](/examples/#person_piqi)
page.

    .include [ .module person ]

    .erlang-type-prefix ""

    .function [
        .name add-person
        .input person
        .error string
    ]

    .function [
        .name get-person
        .input [
            .field [
                .name id
                .type int
            ]
        ]
        .output person
        .error string
    ]

    .function [
        .name list-people
        .output addressbook
    ]

    .list [
        .name addressbook
        .type person
    ]

These are Erlang types from the `addressbook_piqi.hrl` file generated from the
above definitions:

    -type(addressbook() :: [person()]).
    -type(add_person_input() :: person()).
    -type(add_person_error() :: string() | binary()).
    -record(get_person_input, {
        id :: integer()
    }).
    -type(get_person_output() :: person()).
    -type(get_person_error() :: string() | binary()).
    -type(list_people_output() :: addressbook()).

    -type(person() :: #person{}).
    -type(phone_number() :: #phone_number{}).
    -type(get_person_input() :: #get_person_input{}).

Erlang specification for the service implementation functions (from the
`addressbook_piqi_impl.hrl` file):

    -spec add_person/1 :: (add_person_input()) ->
        ok | {error, add_person_error()}.

    -spec get_person/1 :: (get_person_input()) ->
        {ok, get_person_output()} | {error, get_person_error()}.

    -spec list_people/1 :: ('undefined') ->
        {ok, list_people_output()}.

The above functions is what actually Erlang developer needs to implement.

Once the functions are implemented, the code is loaded and the service is
registered, this is how a typical client interaction may look like using the
`piqi call` Piqi-RPC client. The first call is getting command-line help about
the running service.

    $ piqi call http://localhost:8888/addressbook -h
    Piqi-RPC functions (use -p flag for more details):
      add-person -- <person>, which is a combination of:
        --name <string>
        --id <int>
        --email <string> (optional)
        --phone <phone-number> (repeated)
      get-person -- <input>, which is:
        --id <int>

    $ piqi call -t json http://localhost:8888/addressbook/add-person -- \
        --name "J. Random Hacker" \
        --id 0 \
        --email "j.r.hacker@example.com" \
        --phone [ --number "(111) 123 45 67" ] \
        --phone [ \
            --number "(222) 123 45 67" \
            --mobile \
        ]

    $ piqi call -t json http://localhost:8888/addressbook/get-person -- 0
    {
      "name": "J. Random Hacker",
      "id": 0,
      "email": "j.r.hacker@example.com",
      "phone": [
        { "number": "(111) 123 45 67", "type": "home" },
        { "number": "(222) 123 45 67", "type": "mobile" },
        { "number": "(333) 123 45 67", "type": "work" }
    }

### Known limitations

Below is the list of known limitations in the existing implementation of
Piqi-RPC.

-   No logging of user requests

    Currently, Piqi-RPC doesn't support logging of requests or responses. Such
    facility would be useful for debugging Piqi-RPC services and user actions.

Implementation details
----------------------

Piqi-RPC is build on top of Piqi data serialization for Erlang and Piqi data
conversion functionality.

HTTP layer is implemented as a Webmachine behavior.
[Webmachine](http://webmachine.basho.com/) is an excellent HTTP library for
writing well-behaving HTTP services. For instance, it handles general HTTP
workflow, validates headers, performs automatic content-type negotiation, etc.
Webmachine works on top of a popular
[Mochiweb](https://github.com/mochi/mochiweb) HTTP server.

### Piqi-RPC request execution

When a Piqi-RPC call comes in, the following sequence of actions is executed.

1.  The HTTP POST request is validated by Webmachine and Piq-RPC Webmachine
    behavior going through a series of checks: service availability, HTTP
    method, HTTP URL, `Content-Type` and `Accept` headers.

2.  If everything is fine, input parameter (i.e. the body of the HTTP POST
    request) is converted to Protocol Buffers format using Piqi data converter.

3.  Erlang deserialization code, generated from Piqi interface definition, is
    used to read the input parameter represented in Protobuf format into native
    Erlang term representation.

4.  The Erlang implementation callback (the actual users code) is called with
    the input parameter passed as a native Erlang data structure.

5.  The output of the implementation function is serialized to Protocol Buffers
    format using Erlang serialization code, generated from Piqi interface
    definition (symmetric operation to step 3).

6.  Output represented in Protobuf format is converted to the output format
    requested by the caller using Piqi data converter (symmetric operation to
    step 2).

7.  The HTTP response is generated by the Piqi-RPC HTTP layer and sent to the
    caller.

### Piqi data converter

Data conversion between JSON, XML and Protocol Buffers format is performed by an
external `piqi server` program running as Erlang port.

Several instances of `piqi server` can be started on multi-core or multi-CPU
systems in order to increase throughput and decrease latencies. Currently, the
number of workers is configured statically in `piqi` Erlang application.

### Protocol Buffers serialization for Erlang

Protocol Buffers serialization for Erlang is a part of core Piqi functionality.
It is covered in a separate [section](/doc/erlang/) of the current
documentation.

### System portability

Around 80% of Piqi-RPC functionality is multi-format data conversion, which is
implemented in OCaml. The rest of Piqi-RPC is written in Erlang.

The OCaml part runs as external program and communicates with Erlang part via
Erlang port interface (i.e. Unix pipe).

The OCaml data converter -- `piqi server` is packaged as a standalone natively
compiled executable that depends only on `libc`. Everything else is statically
linked in. This makes it easy to build the `piqi` binary once and then just copy
it to any compatible system.

`piqic erlang` is another piece written in OCaml. It is a basic Piqi compiler
that generates Protobuf serialization and deserialization code for Erlang. It is
also packaged as standalone natively compiled binary executable. However, unlike
the `piqi` binary, it doesn't have to be included in the running Piqi-RPC system
as it is only used at the build stage.

OCaml part is very straightforward to build (it doesn't have any external build
dependencies). It successfully builds and runs on 32- and 64-bit Linux, Mac OS
X, Solaris and should work on any other Unix systems and major hardware
architectures.

Additional resources
--------------------

### Erlang Factory SF 2011 presentation slides

Piqi and Piqi-RPC were presented at Erlang Factory conference in San Francisco
in March 2011.

The slides from the presentation may serve as a good introduction to Piqi and
Piqi-RPC.

Note that some slides are out of date and do not reflect the existing
implementation. For instance, since then, the system has been improved to
utilize multiple cores for running data conversion processes.

The slides are available on the [conference
website](http://www.erlang-factory.com/conference/SFBay2011/speakers/AntonLavrik)
(direct link to
[PDF](http://www.erlang-factory.com/upload/presentations/349/anton_lavrik_polyglot_track_day_2.pdf)).
Part of the presentation talking specifically about Piqi-RPC starts from slide
25.
