%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").

%% Module render functions {{{1
main() ->  % {{{2
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
            wf:redirect_to_login("/account");
        error:{change_module, Module} ->
            URI = wf:uri(),
            [_, QS] = string:tokens(URI, "?"),
            wf:redirect(wf:f("/~s?~s", [Module, QS]))
    end.



author(_Page) ->  % {{{2
    "".

description(_Page) ->  % {{{2
    "".

body_attrs(_Page) ->  % {{{2
    "".

head(_Page) ->  % {{{2
    "".

title(#cms_page{title=Title}) ->  % {{{2
    Title.

body(Page) -> % {{{2
    common:parallel_block(Page, "body").
	
%% Event handlers {{{1
event(click) -> % {{{2
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).

%% Block renderers {{{1
maybe_block(_Page, "", _Classes) -> % {{{2
    "";
maybe_block(Page, Block, Classes) -> % {{{2
    #panel{
       html_id=common:block_to_html_id(Block),
       class=Classes,
       body=common:parallel_block(Page, Block)
    }.
block(Page, Block) -> % {{{2
    common:parallel_block(Page, Block).
maybe_redirect_to_login(#cms_page{accepted_role=undefined} = Page) -> % {{{2
    wf:info("Not redirect to login: ~p", [Page]),
    Page;
maybe_redirect_to_login(#cms_page{accepted_role=Role} = Page) -> % {{{2
    wf:info("Redirect to login: ~p", [Page]),
    case wf:role(Role) of 
        true ->
            Page;
        false ->
            error(unauthorized)
    end.

maybe_change_module(#cms_page{module=Module} = Page) -> % {{{2
    wf:info("Change module: ~p", [Page]),
    case wf:page_module() of 
        Module ->
            Page;
        _Other ->
            error({change_module, Module})
    end.
