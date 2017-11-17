%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").

%% CMS Module interface {{{1
description() -> % {{{2
    "Main module".

%% Module render functions {{{1
main() ->  % {{{2
    common:waterfall(#cms_page{}, "router").

author(_Page) ->  % {{{2
    "".

description(Page) ->  % {{{2
    Page#cms_page.description.

body_attrs(_Page) ->  % {{{2
    "".

head(_Page) ->  % {{{2
    "".

title(#cms_page{title=Title}) ->  % {{{2
    Title.

body(Page) -> % {{{2
    wf:state(page, Page),
    common:parallel_block(Page, "body").
	
%% Event handlers {{{1
event({page, construct, PID, [Block|_]=BlocksPath}) -> % {{{2
    wf:update(common:block_to_html_id(Block), common:parallel_block(wf:state(page), Block));
event(Ev) -> % {{{2
    ?LOG("~p event ~p", [?MODULE, Ev]).


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

flash() -> % {{{2
    #flash{}.
