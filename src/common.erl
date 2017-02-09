-module(common).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").

parallel_block(#cms_page{id = PID} = Page, Block) -> % {{{1
    Functions = db:get_mfa(PID, Block),
    lists:map(fun(#cms_mfa{mfa={M, F, Args}}) ->
                        apply(M, F, [Page | Args]);
                   (#cms_mfa{mfa=Fun}) when is_function(Fun) ->
                                    Fun(Page)
                end, 
                Functions).

waterfall(#cms_page{id = PID} = Page, Block) -> % {{{1
    Functions = db:get_mfa(PID, Block),
    wf:info("Waterfall: ~p", [Functions]),
    lists:foldl(fun(#cms_mfa{mfa={M, F, Args}}, P) ->
                        apply(M, F, [P | Args]);
                   (#cms_mfa{mfa=Fun}, P) when is_function(Fun) ->
                                    Fun(P)
                end, 
                Page,
                Functions).

sub_block(Block, Sub) -> % {{{1 % {{{1
    wf:to_list(Block) ++ "/" ++ wf:to_list(Sub).

asset(_Page, AID) -> % {{{1 % {{{1
    Assets = db:get_asset(AID),
    Debug = application:get_env(nitrogen, debug, false),
    #cms_asset{file=Path,
               type=Type} = case {length(Assets), Debug} of
                                {1, _} -> hd(Assets);
                                {L, Debug} when L > 1 ->
                                    hd([A || A <- Assets,
                                             A#cms_asset.minified == not Debug]);
                                    _ -> #cms_asset{type=none}
                                       end,
    case Type of
        script ->
            "<script src='" ++ wf:to_list(Path) ++ "' type='text/javascript' charset='utf-8'></script>";
        css ->
            "<link href='" ++ wf:to_list(Path) ++ "' rel='stylesheet'>";
        image ->
            #image{image=wf:to_list(Path)};
        _ -> []
    end.


template(#cms_page{id=PID}=Page, TID) -> % {{{1
    wf:info("Page for template: ~p, TID: ~p", [Page, TID]),
    [#cms_template{file=File,
                   bindings=Bindings}] = db:get_template(TID),

    #template{file=File,
              bindings=[{'Page', Page} | Bindings]}.

nav_items(Page, Block, Classes) -> % {{{1
    Items = parallel_block(Page, Block),
    #list{body=Items, class=["nav" | Classes], html_id=Block}.

list_item(Page, ItemID) -> % {{{1
    %<li>
    %<a href="index.html"><i class="fa fa-dashboard fa-fw"></i> Dashboard</a>
    %</li>
    #listitem{body=parallel_block(Page, ItemID)}.

link_url(Page, Block, URL) -> % {{{1
    #link{
       url=URL, 
       body=parallel_block(Page, Block)
      }.

link_event(Page, Block, Event) -> % {{{1
    #link{
       actions=Event,
       body=parallel_block(Page, Block)
      }.

icon(_Page, Font, Name, Classes) -> % {{{1
    icon(Font, Name, Classes).

icon(Font, Name, Classes) -> % {{{1
    Cls = [Font, wf:f("~s-~s", [Font, Name]) | Classes],
    Class = string:join(Cls, " "),
    wf:f("<i class='~s'></i>", [Class]).
