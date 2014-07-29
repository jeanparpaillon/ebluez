-module(ebluez_app).

-behaviour(application).

%% Application callbacks
-export([start/0,
	 start/2, 
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    application:start(syntax_tools),
    application:start(compiler),
    application:start(goldrush),
    application:start(lager),
    application:start(sasl),
    application:start(crypto),
    application:start(xmerl),
    application:start(dbus),
    ebluez_sup:start_link().

start(_StartType, _StartArgs) ->
    start().

stop(_State) ->
    ok.
