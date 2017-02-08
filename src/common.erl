-module(common).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").

parallel_block(#cms_page{id = PID} = Page, Block) ->
    Functions = db:get_mfa(PID, Block),
    lists:map(fun(#cms_mfa{mfa={M, F, Args}}) ->
                        apply(M, F, [Page | Args]);
                   (#cms_mfa{mfa=Fun}) when is_function(Fun) ->
                                    Fun(Page)
                end, 
                Functions).

waterfall(#cms_page{id = PID} = Page, Block) ->
    Functions = db:get_mfa(PID, Block),
    wf:info("Waterfall: ~p", [Functions]),
    lists:foldl(fun(#cms_mfa{mfa={M, F, Args}}, P) ->
                        apply(M, F, [P | Args]);
                   (#cms_mfa{mfa=Fun}, P) when is_function(Fun) ->
                                    Fun(P)
                end, 
                Page,
                Functions).

sub_block(Block, Sub) ->
    wf:to_list(Block) ++ "/" ++ wf:to_list(Sub).

css(Page, "css" = Block) ->
    Styles = db:get_assets(Page, Block, css),
    [ 
     "<link href='" ++ wf:to_list(Path) ++ "' rel='stylesheet'>"
     || #cms_asset{type=css, file=Path} <- Styles].
    
script(Page, "script" = Block) ->
    Scripts = db:get_assets(Page, Block, js),
    [ 
    "<script src='" ++ wf:to_list(Path) ++ "' type='text/javascript' charset='utf-8'></script>"
     || #cms_asset{type=css, file=Path} <- Scripts].

template(#cms_page{id=PID}=Page, TID) ->
    wf:info("Page for template: ~p, TID: ~p", [Page, TID]),
    [#cms_template{file=File,
                   bindings=Bindings}] = db:get_template(TID),

    #template{file=File,
              bindings=[{'Page', Page} | Bindings]}.

nav_items(Block, Classes) ->
    Items = parallel_block("admin", Block),
    #list{body=Items, class=["nav" | Classes], html_id=Block}.

list_item(Page, ItemID) ->
    %<li>
    %<a href="index.html"><i class="fa fa-dashboard fa-fw"></i> Dashboard</a>
    %</li>
    #listitem{body=parallel_block(Page, ItemID)}.

link_url(Page, Block, URL) ->
    #link{
       url=URL, 
       body=parallel_block(Page, Block)
      }.

link_event(Page, Block, Event) ->
    #link{
       actions=Event,
       body=parallel_block(Page, Block)
      }.

icon(_Page, Font, Name, Classes) ->
    icon(Font, Name, Classes).

icon(Font, Name, Classes) ->
    Cls = [Font, wf:f("~s-~s", [Font, Name]) | Classes],
    Class = string:join(Cls, " "),
    wf:f("<i class='~s'></i>", [Class]).
