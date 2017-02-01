%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").

main() -> 
    PID = case wf:q(page) of
              undefined -> "index";
              A -> A
          end,
    [Page] = db:get_page(PID),

    try common:waterfall(PID, "page", Page)
    catch
        error:unauthorized -> 
            wf:redirect_to_login("/auth?page=login")
    end.


block(Page, Block) ->
    common:waterfall(Page, Block).

scripts() -> 
    [
     common:parallel_block("admin", "css"),
     common:parallel_block("admin", "script")
    ].


title() -> "LiquidCMS".
	
event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).

maybe_redirect_to_login(#cms_page{accepted_role=undefined} = Page) ->
    wf:info("Redirect to login: ~p", [Page]),
    Page;
maybe_redirect_to_login(#cms_page{accepted_role=Role} = Page) ->
    wf:info("Redirect to login: ~p", [Page]),
    case wf:role(Role) of 
        true ->
            Page;
        false ->
            error(unauthorized)
    end.

maybe_change_module(#cms_page{module=Module} = Page) ->
    wf:info("Change module: ~p", [Page]),
    case wf:page_module() of 
        Module ->
            Page;
        _Other ->
            URI = wf:uri(),
            [_, QS] = string:tokens(URI, "?"),
            wf:redirect(wf:f("/~s?~s", [Module, QS]))
    end.
