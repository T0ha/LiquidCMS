-module(common).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").
-include("records.hrl").

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {parallel_block, "Parallell Group of Blocks"},
     {waterfall, "Waterfall Block"},
     {asset, "Static Asset"},
     {template, "Template"},
     {nav_items, "Navigation Menu Items List"},
     {list_item, "List Item"},
     {link_url, "Link"},
     {icon, "Icon"},
     {script, "Inline Script"},
     {text, "Text with HTML"},
     {full_block, "One Column Row"}
     ].

format_block(template, [TID]) -> % {{{2
    [#cms_template{name=Name}] = db:get_template(TID),
    wf:f("Template: ~s(~p)", [Name, TID]);
format_block(asset, [AID]) -> % {{{2
    [#cms_asset{type=Type, file=File, name=Name}|_] = db:get_asset(AID),
    wf:f("Asset ~s: ~s(~p)", [Type, Name, File]);
format_block(F, A) -> % {{{2
    wf:f("common:~s(~p)", [F, A]).

form_data(template) -> % {{{2
    Templates = db:get_templates(),
    DropdDown = [ {Path, Name} || #{file := Path, name := Name} <- Templates],
    [
     #span{text="Template"},
     #dd{
        id=template,
        %value=template,
        options=DropdDown
       }
    ];
form_data(F) -> % {{{2
    [].

save_block(#cms_mfa{mfa={common, template, []}}=Rec) -> % {{{2
    File = q(template, "templates/index.html"),
    Rec#cms_mfa{mfa={common, template, [File]}}.

%% Block renderers {{{1
parallel_block(#cms_page{id = PID} = Page, Block) -> % {{{2
    Functions = db:get_mfa(PID, Block),
    lists:map(fun(#cms_mfa{mfa={M, F, Args}}) ->
                        apply(M, F, [Page | Args]);
                   (#cms_mfa{mfa=Fun}) when is_function(Fun) ->
                                    Fun(Page)
                end, 
                Functions).

waterfall(#cms_page{id = PID} = Page, Block) -> % {{{2
    Functions = db:get_mfa(PID, Block),
    wf:info("Waterfall: ~p", [Functions]),
    lists:foldl(fun(#cms_mfa{mfa={M, F, Args}}, P) ->
                        apply(M, F, [P | Args]);
                   (#cms_mfa{mfa=Fun}, P) when is_function(Fun) ->
                                    Fun(P)
                end, 
                Page,
                Functions).

asset(_Page, AID) -> % {{{1 % {{{2
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


template(#cms_page{id=PID}=Page, TID) -> % {{{2
    wf:info("Page for template: ~p, TID: ~p", [Page, TID]),
    [#cms_template{file=File,
                   bindings=Bindings}] = db:get_template(TID),

    #template{file=File,
              bindings=[{'Page', Page} | Bindings]}.

nav_items(Page, Block, Classes) -> % {{{2
    Items = parallel_block(Page, Block),
    #list{body=Items, class=["nav" | Classes], html_id=Block}.

list_item(Page, ItemID) -> % {{{2
    %<li>
    %<a href="index.html"><i class="fa fa-dashboard fa-fw"></i> Dashboard</a>
    %</li>
    #listitem{body=parallel_block(Page, ItemID)}.

link_url(Page, Block, URL) -> % {{{2
    #link{
       url=URL, 
       body=parallel_block(Page, Block)
      }.

link_event(Page, Block, Event) -> % {{{2
    #link{
       actions=Event,
       body=parallel_block(Page, Block)
      }.

icon(_Page, Font, Name, Classes) -> % {{{2
    icon(Font, Name, Classes).

icon(Font, Name, Classes) -> % {{{2
    Cls = [Font, wf:f("~s-~s", [Font, Name]) | Classes],
    Class = string:join(Cls, " "),
    wf:f("<i class='~s'></i>", [Class]).

script(_Page, Script) -> % {{{2
    wf:wire(#script{script=Script}).

text(_Page, Text) -> % {{{2
    Text.

full_block(_Page, Body) -> % {{{2
    #bs_row{
       body=#bs_col{
               cols={lg, 12},
               body=Body}}.


%% Helpers {{{1
sub_block(Block, Sub) -> % {{{2
    wf:to_list(Block) ++ "/" ++ wf:to_list(Sub).

q(Id, Default) -> % {{{2
    case wf:q(Id) of
        "" ->
           Default;
        A -> A
    end.
