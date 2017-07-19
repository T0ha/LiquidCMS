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

