%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 22 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(ebluez).
-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

-include("ebluez.hrl").
-include_lib("dbus/include/dbus.hrl").

-export([start_link/0]).

-export([test/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {system,
		manager,
		objects     = undefined   :: []}).

% @doc Start ebluez server
%
% @end
%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%
%%% gen_server callbacks
%%%
init([]) ->
    {ok, Bus} = dbus:get_system_bus(),
    case dbus_bus:get_object_manager(Bus, 'org.bluez', ebluez_manager, self()) of
	{ok, Manager} ->
	    {ok, #state{system=Bus, manager=Manager}};
	{error, Err} ->
	    {stop, Err}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

test() ->
    application:start(ebluez).
