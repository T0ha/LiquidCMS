%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").

main() ->  % {{{1
    PID = case wf:q(page) of
              undefined -> "index";
              A -> A
          end,
    Page = case db:get_page(PID) of
               [P] -> P;
               [] -> #cms_page{id="404"}
           end, 
            

    try common:waterfall(Page, "page")
    catch
        error:unauthorized -> 
            wf:redirect_to_login("/auth?page=login");
        error:{change_module, Module} ->
            URI = wf:uri(),
            [_, QS] = string:tokens(URI, "?"),
            wf:redirect(wf:f("/~s?~s", [Module, QS]))
    end.


block(Page, Block) -> % {{{1
    common:parallel_block(Page, Block).

author(_Page) ->  % {{{1
    "".

description(_Page) ->  % {{{1
    "".

body_attrs(_Page) ->  % {{{1
    "".

head(_Page) ->  % {{{1
    "".

title(#cms_page{title=Title}) ->  % {{{1
    Title.

body(Page) -> % {{{1
    common:parallel_block(Page, "body").
	
event(click) -> % {{{1
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).

maybe_redirect_to_login(#cms_page{accepted_role=undefined} = Page) -> % {{{1
    wf:info("Redirect to login: ~p", [Page]),
    Page;
maybe_redirect_to_login(#cms_page{accepted_role=Role} = Page) -> % {{{1
    wf:info("Redirect to login: ~p", [Page]),
    case wf:role(Role) of 
        true ->
            Page;
        false ->
            error(unauthorized)
    end.

maybe_change_module(#cms_page{module=Module} = Page) -> % {{{1
    wf:info("Change module: ~p", [Page]),
    case wf:page_module() of 
        Module ->
            Page;
        _Other ->
            error({change_module, Module})
    end.
