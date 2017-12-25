%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
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
    maybe_add_editor(Page),
    common:parallel_block(Page, "body").
	
%% Event handlers {{{1
event({?MODULE, links, disable}) -> % {{{2
    case common:q(disable_links, "off") of
        "on" ->
            wf:wire(#script{script="$('a,button').click(function(e) { e.preventDefault(); return false;});"});
        _ ->
            wf:wire(#script{script="$('a,button').unbind('click');"})
    end;

event({page, construct, PID, [Block|_]=BlocksPath}) -> % {{{2
    wf:update("body", common:parallel_block(wf:state(page), "body"));
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

%% Helpers {{{1
maybe_add_editor(Page) -> % {{{2
    maybe_add_editor(Page, wf:role(editor)).

maybe_add_editor(Page, false) -> % {{{2
    ok;
maybe_add_editor(#cms_page{id="admin"}, true) -> % {{{2
    ok;
maybe_add_editor(Page, true) -> % {{{2
    wf:insert_top("body",
                  #panel{
                     class="container-fluid",
                     body=#bs_row{
                             style="height:50px;padding:10px;background-color: #c33;",
                             body=[
                                   #bs_col{
                                      cols=[{md, 9}]
                                     },
                                   #bs_col{
                                      cols=[{md, 2}],
                                      body=[
                                            #checkbox{
                                               id=disable_links,
                                               class=["pull-left"],
                                               label_position=none,
                                               postback={?MODULE, links, disable}
                                              },
                                            #label{
                                               for=disable_links,
                                               text=" Disable links"
                                              }
                                          ]},
                                   #bs_col{
                                      cols=[{md, 1}],
                                      body=#btn{
                                              type=success,
                                              size=xs,
                                              text="Log Out",
                                              postback={auth, logout},
                                              delegate=account
                                             }}
                                  ]}}).
