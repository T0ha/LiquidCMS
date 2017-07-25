%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (router).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

%% CMS Module interface {{{1
description() -> % {{{2
    "CMS main routing collection".

functions() -> % {{{2
    [
     {page, "Default (page) router"},
     {maybe_change_module, "Change render module for page"},
     {qs_page_router, "Change page by QS parameter"},
     {qs_page_router_page, "Page and QS parameter value association pair"},
     {render, "Render page after ruoting chain"}
     ].

format_block(F, A) -> % {{{2
    {wf:f("~p:~s(~p)", [?MODULE, F, A]), undefined}.

form_data(qs_page_router, [_, Block, Param]) -> % {{{2
    [
     {"Block name", {block, Block}},
      {"QS Param name", {param, Param}}
     ];
form_data(F, [_, Block, Classes]) -> % {{{2
    {[], [], Block, Classes};
form_data(F, []) -> % {{{2
    {[], []}.

save_block(#cms_mfa{id={undefined, "router"}, mfa={?MODULE, Fun, [Block, Classes]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [Block, Classes]}}.

%% Module install routines {{{1
default_data() -> % {{{2
    #{cms_mfa => [
                    % Page (routing and auth)
                    #cms_mfa{id={"*", "router"},
                             mfa={router, page, []},
                             sort=1},
                    #cms_mfa{id={"*", "router"},
                             mfa={account, maybe_redirect_to_login, []},
                             sort=2},
                    #cms_mfa{id={"*", "router"},
                             mfa={router, maybe_change_module, []},
                             sort=3},
                    #cms_mfa{id={"*", "router"},
                             mfa={router, render, []},
                             sort=4},
                    #cms_mfa{id={"*", "page"},
                             mfa={common, template, ["templates/main.html"]},
                             sort=1}
                 ]}.
%% Block renderers {{{1
page(Page) -> % {{{2
    PID = common:q(page, "index"),
    page(Page, PID).

page(Page, PID) -> % {{{2
    case db:get_page(PID) of
        [P] -> P;
        [] -> Page
    end. 
qs_page_router(#cms_page{id=Default}=Page, Block, Param) -> % {{{2
    Key = wf:q(Param),
    KV = common:parallel_block(Page, Block),
    PID = proplists:get_value(Key, KV, Default),
    case db:get_page(PID) of
        [P] -> P;
        [] -> Page
    end. 

qs_page_router_page(_Page, K, PID) -> % {{{2
    {K, PID}.

maybe_change_module(#cms_page{module=Module} = Page) -> % {{{2
    wf:info("Change module: ~p", [Page]),
    case wf:page_module() of 
        Module ->
            Page;
        _Other ->
            URI = wf:uri(),
            [_, QS] = string:tokens(URI, "?"),
            wf:redirect(wf:f("/~s?~s", [Module, QS]))
    end.
render(Page) -> % {{{2
    try common:waterfall(Page, "page")
    catch
        error:unauthorized -> 
            wf:redirect_to_login("/account");
        error:{change_module, Module} ->
            URI = wf:uri(),
            [_, QS] = string:tokens(URI, "?"),
            wf:redirect(wf:f("/~s?~s", [Module, QS]))
    end.

%% Event handlers % {{{1
event(Ev) -> % {{{2
    wf:info("~p event ~p", [?MODULE, Ev]).

%% Helpers {{{1

%% Dropdown formatters {{{1
