-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Node = node(),
    mnesia:create_schema([Node]),
    {ok, _}=application:ensure_all_started(lager),
    {ok, _}=application:ensure_all_started(mnesia),
    case lists:member(cms_settings, mnesia:system_info(tables)) of
        false ->
            {atomic, _}=db:install([]),
            EbinDir = filename:dirname(code:which(?MODULE)),
            {ok, Files} = file:list_dir(EbinDir),
            Mods = [wf:to_atom(filename:rootname(F))
                    || F <- Files,
                       filename:extension(F) /= ".app"],
            Modules = [M || M <- Mods,
                            F <- M:module_info(exports),
                            F == {install, 0}],
            io:format("~p~n", [Modules]),
            ok=lists:foreach(fun(M) -> M:install() end, Modules),
            application:set_env(nitrogen, installing, true),
            nitrogen_sup:start_link();
        true ->
            application:set_env(nitrogen, installing, false),
            nitrogen_sup:start_link()
    end.


stop(_State) ->
    ok.
