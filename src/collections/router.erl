%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (router).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

?DESCRIPTION(CMS main routing collection).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {page, "Default (page) router"},
     {maybe_change_module, "Change render module for page"},
     {qs_page_router, "Change page by QS parameter"},
     {role_page_router, "Change page by current user main role"},
     {kv_item, "Page and some (QS or role) parameter value association pair"},
     {render, "Render page after ruoting chain"}
     ].

format_block(F, A) -> % {{{2
    {wf:f("~p:~s(~p)", [?MODULE, F, A]), undefined}.

form_data(qs_page_router, A) -> % {{{2
    [_, Block, Param] = admin:maybe_empty(A, 3),
    [
     {"Block name", {block, Block}},
     {"QS Param name", {param, Param}}
     ];
form_data(role_page_router, A) -> % {{{2
    [_, Block] = admin:maybe_empty(A, 2),
    [
     {"Block name", {block, Block}}
     ];
form_data(kv_item, A) -> % {{{2
    [_, K, V] = admin:maybe_empty(A, 3),
    [
      {"Key", {key, K}},
      {"Page to redirect", {value, V}}
     ];
%form_data(F, [_, Block, Classes]) -> % {{{2
%    {[], [], Block, Classes};
form_data(f, []) -> % {{{2
    {[], []}.

save_block(#cms_mfa{mfa={?MODULE, role_page_router, [Block, Block, _Classes]}}=Rec) -> % {{{2
    Rec#cms_mfa{id={"*", "router"}, mfa={?MODULE, role_page_router, [Block]}};
save_block(#cms_mfa{mfa={?MODULE, qs_page_router, [Block, Block, Param, _Classes]}}=Rec) -> % {{{2
    Rec#cms_mfa{id={"*", "router"}, mfa={?MODULE, qs_page_router, [Block, Param]}};
save_block(#cms_mfa{mfa={?MODULE, kv_item, [_Block, K, V, _Classes]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, kv_item, [K, V]}};
save_block(#cms_mfa{mfa={?MODULE, Fun, [_, Block, Classes]}}=Rec) -> % {{{2
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

kv_item(Page, K, PID) -> % {{{2
    common:kv_item(Page, K, PID).

role_page_router(Page, Block) -> % {{{2
    #cms_user{role=Role} = account:user(),
    page_from_kv(Page, Block, wf:to_list(Role)).

qs_page_router(#cms_page{id=Default}=Page, Block, Param) -> % {{{2
    Key = wf:q(Param),
    wf:info("Key: ~p", [Key]),
    page_from_kv(Page, Block, Key).

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
page_from_kv(#cms_page{id=Default}=Page, Block, Key) -> % {{{2
    wf:info("Key: ~p", [Key]),
    KV = common:parallel_block(Page, Block),
    wf:info("KV: ~p", [KV]),
    V = proplists:get_value(wf:to_list(Key), KV, Default),
    wf:info("V: ~p", [V]),
    page(Page, V).

%% Dropdown formatters {{{1
