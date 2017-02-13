%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (admin).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

%% Module render functions {{{1
main() -> 
    index:main().

title() -> "Liquid CMS - Admin".

body(Page) ->
    index:body(Page).

%% Module install routines {{{1
default_data() -> % {{{2
    #{cms_mfa => [
                    %Scripts
                    #cms_mfa{id={"*", "script"},
                             mfa={common, asset, [["js", "jquery"]]},
                             sort=1},
                    #cms_mfa{id={"*", "script"},
                             mfa={common, asset, [["js", "jquery-ui"]]},
                             sort=2},
                    #cms_mfa{id={"*", "script"},
                             mfa={common, asset, [["js", "bert"]]},
                             sort=3},
                    #cms_mfa{id={"*", "script"},
                             mfa={common, asset, [["js", "nitrogen"]]},
                             sort=4},
                    #cms_mfa{id={"*", "script"},
                             mfa={common, asset, [["js", "livevalidation"]]},
                             sort=5},

                    %CSS
                    #cms_mfa{id={"*", "css"},
                             mfa={common, asset, [["css", "jquery-ui"]]},
                             sort=1},
                    #cms_mfa{id={"*", "css"},
                             mfa={common, asset, [["css", "bootstrap"]]},
                             sort=2}
                      ]}.

install() -> % {{{2
    {ok, StaticFolders} = application:get_env(simple_bridge, static_paths),
    %io:format("StaticFolders: ~p~n", [StaticFolders]),
    {ok, OldCWD} = file:get_cwd(),
    file:set_cwd("static"),
    Static = [{wf:f("~s", [Path]), Files} ||
              Path <- StaticFolders,
              filelib:is_dir(Path),
              {ok, Files} <- [file:list_dir(Path)],
              Path /= "nitrogen/"],
    io:format("Static: ~p~n", [Static]),
    Assets = lists:foldl(fun({Path, Files}, A) ->
                                 [file_to_asset(File, Path) || File <- Files, File /= ".empty"] ++ A
                         end,
                         [],
                         Static),
    %io:format("Assets: ~p~n", [Assets]),
    NitrogenStatic = [
                      {"nitrogen/", 
                       ["bert.js", "bert.min.js",
                        %"jquery.js", "jquery.min.js",
                        "livevalidation.js", "livevalidation.min.js",
                        "nitrogen.js", "nitrogen.min.js", "nitrogen.css"]},
                      {"nitrogen/jquery-ui/", 
                       ["jquery-ui.js", "jquery-ui.min.js",
                        "jquery-ui.css", "jquery-ui.min.css"]}
                     ],
    NitrogenAssets = [
                      file_to_asset(File, Path) || {Path, Files} <- NitrogenStatic, File <- Files],
    db:save(Assets ++ NitrogenAssets),
    file:set_cwd(OldCWD),
    {ok, TemplateFiles} = file:list_dir("templates"),
    lists:foreach(fun(F) -> add_template("templates/" ++ F, []) end, TemplateFiles),
    add_page("index", "templates/index.html"),
    add_page("admin", "templates/blank.html", admin, admin),
    add_to_block("admin", "css", {asset, ["css", "font-awesome"]}, 3),
    add_to_block("admin", "css", {asset, ["css", "metisMenu"]}, 4),
    add_to_block("admin", "css", {asset, ["css", "sb-admin-2"]}, 6),
    add_to_block("admin", "css", {asset, ["css", "admin"]}, 7),

    add_to_block("admin", "script", {asset, ["js", "bootstrap"]}, 6),
    add_to_block("admin", "script", {asset, ["js", "metisMenu"]}, 7),
    add_to_block("admin", "script", {asset, ["js", "sb-admin-2"]}, 8),
    add_navbar_button("admin", "sidebar-nav", "assets", {{"fa", "hdd-o", []}, "Static Assets"}, {menu, "static-assets-menu"}),
    add_navbar_button("admin", "static-assets-menu", "assets-css", {{"fa", "css3", []}, "CSS"}, {event, ?POSTBACK({asset, show, css, 1, 10})}),
    add_navbar_button("admin", "static-assets-menu", "assets-scripst", {{"fa", "code", []}, "JavaScript"}, {event, ?POSTBACK({asset, show, script, 1, 10})}),
    add_navbar_button("admin", "static-assets-menu", "assets-img", {{"fa", "image", []}, "Images"}, {event, ?POSTBACK({asset, show, images, 1, 10})}),
    add_navbar_button("admin", "static-assets-menu", "assets-binary", {{"fa", "file-o", []}, "Other"}, {event, ?POSTBACK({asset, show, binary, 1, 10})}),
    add_to_block("admin", "container", {template, "templates/datatables.html"}, 1),
    ok.

%% Different components adding to pages  {{{1
add_page(PID, TemplatePath) -> % {{{2
   add_page(PID, TemplatePath, undefined, index).

add_page(PID, TemplatePath, Role, Module) -> % {{{2
    Funs = [
            %fun index:maybe_redirect_to_login/1,
            fun index:maybe_change_module/1,
            fun(Page) -> common:template(Page, "templates/main.html") end
           ],
    NFuns = lists:zip(lists:seq(1, length(Funs)), Funs),
    PageMFAs = [#cms_mfa{
               id = {PID, "page"},
               mfa=F,
               sort=N} || {N, F} <- NFuns],
    MFAs = [#cms_mfa{
               id = {PID, "body"},
               mfa= {common, template, [TemplatePath]},
               sort=1} | PageMFAs],
    Page = #cms_page{
              id=PID,
              accepted_role=Role,
              module=Module
             },
    db:save([Page | MFAs]).

add_template(TemplatePath, Bindings) -> % {{{2
    IsTemplate = filelib:is_regular(TemplatePath),
    if IsTemplate ->
           db:save(#cms_template{
                      file = TemplatePath,
                      bindings = Bindings});
       true ->
           {error, no_template}
    end.

add_to_block(PID, Block, Mater)  -> % {{{2
    add_to_block(PID, Block, Mater, 1).

add_to_block(PID, Block, {Type, ID}, Sort) -> % {{{2
    db:save(#cms_mfa{
               id={PID, Block},
               mfa={common, Type, [ID]},
               sort=Sort}).

add_navbar_button(PID, MenuBlock, ItemBlock, {Icon, Text}, {menu, SubMenuBlock}) -> % {{{2
    ItemLinkBlock = common:sub_block(ItemBlock, "link"),
    ItemSubmenuBlock = common:sub_block(ItemBlock, "submenu"),
    ButtonMFAs = [
                  #cms_mfa{id={PID, MenuBlock},
                           mfa={common,
                                list_item,
                                [ItemBlock]},
                           sort=1},
                  #cms_mfa{id={PID, "body"},
                           mfa=fun(_Page) -> 
                                             wf:wire(#script{script=wf:f("$('#~s').metisMenu();", [MenuBlock])})
                               end,
                           sort=20},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={common,
                                link_url,
                                [ItemLinkBlock, ""]},
                           sort=1},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={common,
                                nav_items,
                                [SubMenuBlock, ["nav-second-level", "collapse"]]},
                           sort=2}
                 ],
    LinkMFAs = [
                #cms_mfa{
                   id={PID, ItemLinkBlock},
                   mfa={common, icon, ["fa", "", ["arrow"]]},
                   sort=3}
                | link_body_funs(PID, ItemLinkBlock, Icon, Text)],
    db:save(ButtonMFAs ++ LinkMFAs);
add_navbar_button(PID, MenuBlock, ItemBlock, {Icon, Text}, {event, Actions}) -> % {{{2
    ItemLinkBlock = common:sub_block(ItemBlock, "link"),
    ButtonMFAs = [
                  #cms_mfa{id={PID, MenuBlock},
                           mfa={common,
                                list_item,
                                [ItemBlock]},
                           sort=1},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={common,
                                link_event,
                                [ItemLinkBlock, Actions]},
                           sort=1}
                 ],
    LinkMFAs = link_body_funs(PID, ItemLinkBlock, Icon, Text),
    db:save(ButtonMFAs ++ LinkMFAs);
add_navbar_button(PID, MenuBlock, ItemBlock, {Icon, Text}, {url, URL}) -> % {{{2
    ItemLinkBlock = common:sub_block(ItemBlock, "link"),
    ButtonMFAs = [
                  #cms_mfa{id={PID, MenuBlock},
                           mfa={common,
                                list_item,
                                [ItemBlock]},
                           sort=1},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={common,
                                link_url,
                                [ItemLinkBlock, URL]},
                           sort=1}
                 ],
    LinkMFAs = link_body_funs(PID, ItemLinkBlock, Icon, Text),
    db:save(ButtonMFAs ++ LinkMFAs).

%% Helpers and private functions {{{1
menu_item_funs(Page, MenuBlock, ItemSub, URL) -> % {{{2
    ItemBlock = common:sub_block(MenuBlock, ItemSub),
    ItemBodyBlock = common:sub_block(ItemBlock, "body"),

    [
     #cms_mfa{id={Page, MenuBlock},
              mfa={common,
                   list_item,
                   [ItemBlock]},
              sort=1},
     #cms_mfa{id={Page, ItemBlock},
              mfa={common,
                   link_url,
                   [ItemBodyBlock, URL]},
              sort=1}
    ].

link_body_funs(PID, LinkBlock, Icon, Text) -> % {{{2
    [
     case Icon of
         {_Font, _Name, _Classes} = Args ->
             A = tuple_to_list(Args),
             #cms_mfa{id={PID, LinkBlock},
                      mfa={common, icon, A},
                      sort=1};
         _ ->
             []
     end,
     if Text /= undefined ->
            #cms_mfa{id={PID, LinkBlock},
                     mfa=fun(_Page) -> " " ++ Text end,
                     sort=2};
        true -> []
     end
    ].

file_to_asset(File, Path) -> % {{{2
        [Ext, Min | Id] = lists:reverse(string:tokens(File, ".")),
        case {Ext, Min} of
            {"js", "min"} ->
                #cms_asset{
                   id=[Ext | Id],
                   name=string:join(lists:reverse(Id), "."),
                   description=string:join(lists:reverse(Id), "."),
                   file=filename:join([Path, File]),
                   minified=true,
                   type=script};
            {"js", _} ->
                #cms_asset{
                   id=[Ext, Min | Id],
                   name=string:join(lists:reverse([Min|Id]), "."),
                   description=string:join(lists:reverse([Min|Id]), "."),
                   file=filename:join([Path, File]),
                   type=script};
            {"css", "min"} ->
                #cms_asset{
                   id=[Ext | Id],
                   name=string:join(lists:reverse(Id), "."),
                   description=string:join(lists:reverse(Id), "."),
                   file=filename:join([Path, File]),
                   minified=true,
                   type=css};
            {"css", _} ->
                #cms_asset{
                   id=[Ext, Min | Id],
                   name=string:join(lists:reverse([Min|Id]), "."),
                   description=string:join(lists:reverse([Min|Id]), "."),
                   file=filename:join([Path, File]),
                   type=css};
            {Any, _} ->
                #cms_asset{
                   id=[Ext, Min | Id],
                   name=string:join(lists:reverse([Min|Id]), "."),
                   description=string:join(lists:reverse([Min|Id]), "."),
                   file=filename:join([Path, File]),
                   type=binary}
        end.

assets_table_rows(Assets) -> % {{{2
    [assets_table_row(A) || A <- Assets].

assets_table_row(#cms_asset{ % {{{2
                    id=Id,
                    name=Name,
                    description=Description,
                    file=Path,
                    minified=Minified,
                    type=Type
                   }=Asset) ->
    #tablerow{
       cells=[
              #tablecell{text=""},
              #tablecell{body=#inplace_textbox{tag={asset, Asset, name}, text=Name}},
              #tablecell{body=#inplace_textbox{tag={asset, Asset, description},text=Description}},
              #tablecell{text=Path},
              #tablecell{text=Minified},
              #tablecell{text=Type}
             ]}.


%% Event handlers {{{1
event({asset, show, Type, Start, Count}) -> % {{{2
    Assets = case db:get_assets(Type) of
                 A when length(A) =< Count -> A;
                 A when length(A) - Start >= Count ->
                     lists:sublist(A, Start, Count);
                 A ->
                     C = length(A) - Start,
                     wf:f("~p", [C]),
                     lists:sublist(A, Start, C)
             end,
    Table = #table{id=assets,
                   class=["table-striped", "table-bordered", "table-hover"],
                   rows=[
                         #tablerow{
                            cells=[
                                   #tableheader{text="N"},
                                   #tableheader{text="Name"},
                                   #tableheader{text="Description"},
                                   #tableheader{text="Path"},
                                   #tableheader{text="Minified"},
                                   #tableheader{text="Type"}
                                  ]}| assets_table_rows(Assets)]
                  },
    wf:update(container, [
                           #h1{text=wf:f("Static Assets: ~s", [Type])},
                           #bs_row{
                              body=#bs_col{
                                      cols={lg, 12},
                                      body=Table
                                     }
                             }]);
event(Ev) -> % {{{2
    wf:info("~p event ~p", [?MODULE, Ev]).

inplace_textbox_event({asset, Record, Field}, Value) ->
    Val = db:update(Record, Field, Value),
    Val;
inplace_textbox_event(Tag, Value) ->
    wf:info("~p inplace tb event ~p: ~p", [?MODULE, Tag, Value]),
    Value.

api_event(Name, Tag, Args) -> % {{{2
    wf:info("~p API event ~p(~p; ~p)", [?MODULE, Name, Tag, Args]).
