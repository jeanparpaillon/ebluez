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
-include_lib("dbus/include/dbus_client.hrl").

-export([start_link/0,
	 get_objects/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([test/0]).

-define(SERVER, ?MODULE).

-record(state, {bus,
		objects = [] :: list()
	       }).

% @doc Start ebluez client
%
% @end
%
start_link() ->
    dbus_client:start_link(system, {gen_server, ?MODULE}, [{manager, 'org.bluez'}], []).

-spec get_objects(pid()) -> list().
get_objects(Ref) ->
    gen_server:call(Ref, get_objects).

test() ->
    application:start(ebluez).

%%%
%%% gen_server callbacks
%%%
init([Bus, []]) ->
    lager:debug("Init ebluez..."),
    {ok, #state{bus=Bus}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(get_objects, _From, #state{objects=Objects}=State) ->
    {reply, Objects, State};

handle_call({signal, _, 'org.freedesktop.DBus.ObjectManager', 'InterfacesAdded', [Path, Ifaces]}, 
	    _From, #state{objects=Objects}=State) ->
    {reply, ok, State#state{objects=add_object(Path, Ifaces, Objects)}};

handle_call({signal, _, 'org.freedesktop.DBus.ObjectManager', 'InterfacesRemoved', [Path, Ifaces]}, 
	    _From, #state{objects=Objects}=State) ->
    {reply, ok, State#state{objects=add_object(Path, Ifaces, Objects)}};

handle_call({signal, Path, Iface, Signal, _Args}, _From, State) ->
    lager:debug("Unhandled signal: ~p:~p on ~p~n", [Iface, Signal, Path]),
    {reply, ok, State};

handle_call({add_objects, Objects}, _From, State) ->
    {reply, ok, State#state{objects=Objects}};

handle_call(_Msg, _From, State) ->
    io:format("handle_call(~p)~n", [_Msg]),
    {reply, ignore, State}.

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

%%%
%%% Priv
%%%
add_object(_Path, [], Acc) ->
    Acc;
add_object(Path, [{IfaceName, Props} | Ifaces], Acc) ->
    lager:debug("New object: ~p~n", [Path]),
    case gb_trees:lookup(IfaceName, Acc) of
	none ->
	    Acc2 = gb_trees:insert(IfaceName, [{Path, Props}], Acc),
	    add_object(Path, Ifaces, Acc2);
	{value, Objects} ->
	    Acc2 = gb_trees:update(IfaceName, [{Path, Props} | Objects], Acc),
	    add_object(Path, Ifaces, Acc2)
    end.
