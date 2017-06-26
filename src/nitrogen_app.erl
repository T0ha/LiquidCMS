-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok ->
            {ok, _}=application:ensure_all_started(lager),
            {ok, _}=application:ensure_all_started(mnesia),
            {atomic, _}=db:install([]),
            {ok, Files} = file:list_dir("ebin"),
            Mods = [wf:to_atom(filename:rootname(F)) || F <- Files, filename:extension(F) /= ".app"],
            Modules = [M || M <- Mods,
                            F <- M:module_info(exports),
                            F == {install, 0}],
            ok=lists:foreach(fun(M) -> M:install() end, Modules),
            application:set_env(nitrogen, installing, true),
            nitrogen_sup:start_link();
        {error, _} ->
            {ok, _}=application:ensure_all_started(mnesia),
            admin:install(),
            application:set_env(nitrogen, installing, false),
            nitrogen_sup:start_link()
    end.


stop(_State) ->
    ok.
