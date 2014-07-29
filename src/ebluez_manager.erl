%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(ebluez_manager).
-compile({parse_transform, lager_transform}).

-include_lib("dbus/include/dbus.hrl").

-behaviour(dbus_objects_manager).

%%
%% dbus_objects_manager callbacks
%%
-export([init/2,
	 add_objects/2]).

-record(state, {manager :: dbus_proxy(), 
		srv     :: pid()}).

init(Manager, Srv) ->
    {ok, #state{manager=Manager, srv=Srv}}.

add_objects([], State) ->
    {ok, State};
add_objects([{Path, _Ifaces} | Objects], State) ->
    lager:debug("New object ~p", [Path]),
    add_objects(Objects, State).
