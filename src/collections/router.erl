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
     {render, "Render page after ruoting chain"},
     {common_redirect, "Redirect to select URL"}
     ].

format_block(F, A) -> % {{{2
    {wf:f("~p:~s(~p)", [?MODULE, F, A]), undefined}.
form_data(common_redirect, A) -> % {{{2
    [_, Block, URL] = admin:maybe_empty(A, 3),
    [ {"Block name", {block, Block}},
      {"URL", {url, URL}}
    ]
    ;
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

save_block(#cms_mfa{mfa={?MODULE, role_page_router, [Block, Block, _Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{id={"*", "router"}, mfa={?MODULE, role_page_router, [Block]}};
save_block(#cms_mfa{mfa={?MODULE, qs_page_router, [Block, Block, Param, _Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{id={"*", "router"}, mfa={?MODULE, qs_page_router, [Block, Param]}};
save_block(#cms_mfa{mfa={?MODULE, common_redirect, [_, Block, Param, _Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, common_redirect, [Block, Param]}};
save_block(#cms_mfa{mfa={?MODULE, kv_item, [_Block, K, V, _Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, kv_item, [K, V]}};
save_block(#cms_mfa{mfa={?MODULE, Fun, [Block, _Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [Block]}}.

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

role_page_router(#cms_page{id=PID}=Page, Block) when PID == undefined; % {{{2
                                                     PID == "index" ->
    #cms_user{role=Role} = account:user(),
    page_from_kv(Page, Block, wf:to_list(Role));
role_page_router(Page, _Block) -> % {{{2
    Page.

qs_page_router(#cms_page{id=_Default}=Page, Block, Param) -> % {{{2
    Key = wf:q(Param),
    ?LOG("~nKey: ~p", [Key]),
    page_from_kv(Page, Block, Key).

common_redirect(_Page, _Block, URL) -> % {{{2
    ?LOG("URL: ~p", [URL]),
    case URL of
        []  -> undefined;
        URL -> wf:redirect(URL)
    end.
maybe_change_module(#cms_page{module=Module} = Page) -> % {{{2
    maybe_change_module(#cms_page{module=Module} = Page, []).
maybe_change_module(#cms_page{module=Module} = Page, _) -> % {{{2
    ?LOG("Change module: ~p(~p)", [Page, Module]),
    case wf:page_module() of 
        Module ->
            Page;
        _Other ->
            URI = wf:uri(),
            QS = get_qs(URI),
            wf:redirect(wf:f("/~s~s", [Module, QS]))
    end.
render(Page, _) -> % {{{2
    render(Page).
render(Page) -> % {{{2
    ?LOG("render:: ~p", [Page]),
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
    ?LOG("~p event ~p", [?MODULE, Ev]).

%% Helpers {{{1
page_from_kv(#cms_page{id=Default}=Page, Block, Key) -> % {{{2
    ?LOG("Key: ~p", [Key]),
    KV = common:parallel_block(Page, Block),
    ?LOG("KV: ~p", [KV]),
    V = proplists:get_value(wf:to_list(Key), KV, Default),
    ?LOG("V: ~p", [V]),
    page(Page, V).

get_qs(URI) -> % {{{2
    maybe_qs(string:tokens(URI, "?")).

maybe_qs([_, QS]) -> % {{{2
    "?" ++ QS;
maybe_qs(_) -> % {{{2
    "".

%% Dropdown formatters {{{1
