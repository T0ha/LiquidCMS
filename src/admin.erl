%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (admin).
-compile([export_all, {parse_transform, lager_transform}]).
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
    lager:info("Installing ~p module", [?MODULE]),
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
    %add_page("index", "templates/index.html"),
    add_page("admin", "templates/blank.html", admin, admin),
    add_to_block("admin", "css", {asset, ["css", "font-awesome"]}, 3),
    add_to_block("admin", "css", {asset, ["css", "metisMenu"]}, 4),
    add_to_block("admin", "css", {asset, ["css", "sb-admin-2"]}, 6),
    add_to_block("admin", "css", {asset, ["css", "admin"]}, 7),

    add_to_block("admin", "script", {asset, ["js", "bootstrap"]}, 6),
    add_to_block("admin", "script", {asset, ["js", "metisMenu"]}, 7),
    add_to_block("admin", "script", {asset, ["js", "sb-admin-2"]}, 8),
    add_to_block("admin", "script", {asset, ["js", "hotkeys", "jquery"]}, 9),
    add_to_block("admin", "script", {asset, ["js", "bootstrap-wysiwyg"]}, 10),

    add_navbar_button("admin", "sidebar-nav", "assets", {{"fa", "hdd-o", []}, "Static Assets"}, {menu, "static-assets-menu"}),
    add_navbar_button("admin", "static-assets-menu", "assets-css", {{"fa", "css3", []}, "CSS"}, {event, ?POSTBACK({asset, show, css})}),
    add_navbar_button("admin", "static-assets-menu", "assets-scripst", {{"fa", "code", []}, "JavaScript"}, {event, ?POSTBACK({asset, show, script})}),
    add_navbar_button("admin", "static-assets-menu", "assets-img", {{"fa", "image", []}, "Images"}, {event, ?POSTBACK({asset, show, image})}),
    add_navbar_button("admin", "static-assets-menu", "assets-binary", {{"fa", "file-o", []}, "Other"}, {event, ?POSTBACK({asset, show, binary})}),

    add_navbar_button("admin", "sidebar-nav", "templates", {{"fa", "hdd-o", []}, "Templates"}, {event, ?POSTBACK({template, show})}),

    add_navbar_button("admin", "sidebar-nav", "pages", {{"fa", "file-o", []}, "Pages"}, {menu, "pages-menu"}),
    add_navbar_button("admin", "pages-menu", "pages-all", {{"fa", "file-o", []}, "All Pages"}, {event, ?POSTBACK({page, show})}),
    add_navbar_button("admin", "pages-menu", "page-construct", {{"fa", "puzzle-piece", []}, "Construct Page"}, {event, ?POSTBACK({page, construct})}),

    ok.

%% Different components adding to pages  {{{1
add_page(PID, TemplatePath) -> % {{{2
   add_page(PID, TemplatePath, undefined, index).

add_page(PID, TemplatePath, Role, Module) -> % {{{2
    lager:info("Installing ~p module", [?MODULE]),
    add_page(PID, <<"LiquidCMS">>, "LiquidCMS", Role, Module),
    add_to_block(PID, "body", {template, TemplatePath}).

add_page(PID, Title, Description, Role, Module) -> % {{{2

    Funs = [
            {index, maybe_redirect_to_login, []},
            {index, maybe_change_module, []},
            {common, template, ["templates/main.html"]}
           ],
    NFuns = lists:zip(lists:seq(1, length(Funs)), Funs),
    MFAs = [#cms_mfa{
               id = {PID, "page"},
               mfa=F,
               sort=N} || {N, F} <- NFuns],
    Page = #cms_page{
              id=PID,
              title=Title,
              description=Description,
              accepted_role=Role,
              module=Module
             },
    db:save([Page | MFAs]).

add_template(TemplatePath, Bindings) -> % {{{2
    add_template(TemplatePath, TemplatePath, TemplatePath, Bindings).

add_template(Name, Description, TemplatePath, Bindings) -> % {{{2
    IsTemplate = filelib:is_regular(TemplatePath),
    if IsTemplate ->
           db:save(#cms_template{
                      file = TemplatePath,
                      name = Name,
                      description = Description,
                      bindings = Bindings});
       true ->
           {error, no_template}
    end.

add_to_block(PID, Block, Mater)  -> % {{{2
    add_to_block(PID, Block, Mater, 1).

add_to_block(PID, Block, {Type, ID}, Sort) -> % {{{2
    add_to_block(PID, Block, {common, Type, [ID]}, Sort);
add_to_block(PID, Block, {M, F, A}, Sort) -> % {{{2
    db:save(#cms_mfa{
               id={PID, Block},
               mfa={M, F, A},
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
                           mfa={common, script, [wf:f("$('#~s').metisMenu();", [MenuBlock])]},
                           sort=20},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={common,
                                link_url,
                                [ItemLinkBlock, ""]},
                           sort=1},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={bootstrap,
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
                     mfa={common, text, [" " ++ Text]},
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

maybe_set(Id, Val) -> % {{{2
    case wf:q(Id) of
        undefined ->
            wf:set(Id, Val);
        "" ->
            wf:set(Id, Val);
        A ->
            wf:info("~s vaue is ~p", [Id, A])
    end.

update_container(Header, ButtonText, ButtonPostBack, Body) -> % {{{2
    wf:update(container, [
                           #bs_row{
                              body=[
                                    #bs_col{
                                      cols={lg, 10},
                                      body=#h1{text=Header}
                                              },
                                    #bs_col{
                                      cols={lg, 2},
                                      body=#button{
                                              text=ButtonText,
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK(ButtonPostBack)
                                             }}
                                   ]},
                           #bs_row{
                              body=#bs_col{
                                      cols={lg, 12},
                                      body=Body
                                     }
                             }]).

new_modal(Title, SavePoctback, UploadTag, Form) -> % {{{2
    Bottom = #btn{
                type=success,
                size=md,
                text="Save",
                postback=SavePoctback
               },
    BodyCols = case UploadTag of
                   undefined -> {lg, 12};
                   _ -> {lg, 6}
               end,

    Body = #bs_row{
              body=[
                    #bs_col{
                       show_if=(UploadTag /= undefined),
                       cols={lg, 6},
                       body=#upload{
                               tag=UploadTag,
                               droppable=true,
                               show_button=false,
                               overall_progress=true
                              }},
                    #bs_col{
                       cols=BodyCols,
                       body=Form}
                   ]},
    coldstrap:modal(Title, Body, Bottom, [{has_x_button, true}]).

format_block(#cms_mfa{ % {{{2$
                id={PID, _},
                mfa={index, maybe_redirect_to_login, []}
               }=B) ->
    [#cms_page{accepted_role=Name}] = db:get_page(PID),
    #sortitem{
       tag={block, PID, B},
       class="well",
       body=wf:f("Grant access to  '~s' page for '~s' only", [PID, Name])};
format_block(#cms_mfa{ % {{{2$
                id={PID, _},
                mfa={index, maybe_change_module, []}
               }=B) ->
    [#cms_page{module=Name}] = db:get_page(PID),
    #sortitem{
       tag={block, PID, B},
       class="well",
       body=wf:f("Use '~s' module for '~s' page", [Name, PID])};

format_block(#cms_mfa{ % {{{2$
                id={PID, Name},
                mfa={M, F, A}
               }=B) ->
    Body = try apply(M, format_block, [F, A])
           catch
              _ -> wf:f("~p, ~p(~p)", [Name, F, A])
           end,

    #sortitem{
       tag={block, PID, B},
       class="well",
       body=[
             Body,
             #span{
                class="pull-right",
                body=[
                      #link{
                         class="btn btn-link",
                         body=common:icon("fa", "pencil", []),
                         actions=?POSTBACK({block, edit, B})
                        },
                      #link{
                         class="btn btn-link",
                         body=common:icon("fa", "remove", []),
                         actions=?POSTBACK({block, remove, B})
                        }
                     ]}
            ]}.

add_default_fields({Data, Formatting}) -> % {{{2
    add_default_fields(Data, Formatting).

add_default_fields(Data, Formatting) -> % {{{2
    [ 
     {"Data", [
               {"Block name", block}
               | Data]},
     {"Formatting",
      Formatting ++ [{"Additional classes", classes}]}
      ].

render_fields(Cols) -> % {{{2
    Width = 12 / length(Cols),
    [
     #bs_row{
        body=[render_field(Col, Width) || Col <- Cols]
       }
    ].

render_field(Any) -> % {{{2
    render_field(Any, 12).

render_field({Label, ID}, Width) when is_atom(ID) -> % {{{2
    render_field({Label, {ID, ""}}, Width);
render_field({Label, {ID, Text}}, Width) -> % {{{2
    #bs_col{
       cols={lg, Width},
       body=[
             #span{text=Label},
             #txtarea{
                id=ID,
                text=Text
               }
            ]};
render_field({Label, Any}, Width) when is_list(Any) -> % {{{2
    #bs_col{
       cols={lg, 12},
       body=[
             #span{text=Label},
             render_fields(Any)
            ]};
render_field({Label, Any}, Width) -> % {{{2
    #bs_col{
       cols={lg, Width},
       body=[
     #span{text=Label},
     Any
    ]};
render_field(Any, Width) -> % {{{2
    #bs_col{
       cols={lg, Width},
       body=Any
      }.

%% Event handlers {{{1
event({asset, new, Type}) -> % {{{2
    new_modal("Upload Static Asset",
              {asset, save},
              asset, 
              [
               #span{text="Name"},
               #txtbx{id=name,
                      placeholder="Asset name"},

               #span{text="Description"},
               #txtarea{id=description,
                        placeholder="Description here..."},

               #hidden{id=path},

               #span{text="Type"},
               #dd{
                  id=type,
                  value=binary,
                  options=common:asset_types()
                 }
              ]);
event({asset, save}) -> % {{{2
    Type = wf:to_atom(wf:q(type)),
    Path = wf:q(path),
    Fname = filename:basename(Path),
    Dir = filename:dirname(Path),
    Asset = file_to_asset(Fname, Dir),
    db:save(Asset#cms_asset{
              name=wf:q(name),
              description=wf:q(description),
              type=Type
             }),
    coldstrap:close_modal(),
    wf:wire(#event{postback={asset, show, Type}});
event({asset, show, Type}) -> % {{{2
    CRUD = #crud{
       pagination_class=["btn", "btn-default"],
       button_class=["btn", "btn-link"],
       table_class=["table-striped", "table-bordered", "table-hover"],
       start=0,
       count=10,
       cols=[
             {name, "Name", tb},
             {description, "Description", ta},
             {file, "Path", none},
             {minified, "Minified", none},
             {type, "Type", {select, common:asset_types()}}
            ],
       funs=#{
         list => fun() -> db:get_assets(Type) end,
         update => fun db:update_map/1, 
         delete => fun db:delete/1
        }
      },
    wf:update(container, [
                           #bs_row{
                              body=[
                                    #bs_col{
                                      cols={lg, 10},
                                      body=#h1{text=wf:f("Static Assets: ~s", [Type])}
                                              },
                                    #bs_col{
                                      cols={lg, 2},
                                      body=#button{
                                              text="Upload Asset",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({asset, new, Type})
                                             }}
                                   ]},
                           #bs_row{
                              body=#bs_col{
                                      cols={lg, 12},
                                      body=CRUD
                                     }
                             }]);
event({template, new}) -> % {{{2
    new_modal("Upload Template",
              {template, save},
              template,
              [
               #span{text="Name"},
               #txtbx{id=name,
                      placeholder="Template name"},

               #span{text="Description"},
               #txtarea{id=description,
                        placeholder="Description here..."},

               #hidden{id=path}
              ]);

event({template, save}) -> % {{{2
    Path = wf:q(path),
    add_template(wf:q(name), wf:q(description), Path, []),
    coldstrap:close_modal(),
    wf:wire(#event{postback={template, show}});
event({template, show}) -> % {{{2
    CRUD = #crud{
       pagination_class=["btn", "btn-default"],
       button_class=["btn", "btn-link"],
       table_class=["table-striped", "table-bordered", "table-hover"],
       start=0,
       count=10,
       cols=[
             {name, "Name", tb},
             {description, "Description", ta},
             {file, "Path", none}
             %{bindings, "Bindings", none}
            ],
       funs=#{
         list => fun db:get_templates/0,
         update => fun db:update_map/1, 
         delete => fun db:delete/1
        }
      },
    wf:update(container, [
                           #bs_row{
                              body=[
                                    #bs_col{
                                      cols={lg, 10},
                                      body=#h1{text="Templates"}
                                              },
                                    #bs_col{
                                      cols={lg, 2},
                                      body=#button{
                                              text="Upload Template",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({template, new})
                                             }}
                                   ]},
                           #bs_row{
                              body=#bs_col{
                                      cols={lg, 12},
                                      body=CRUD
                                     }
                             }]);
event({page, show}) -> % {{{2
    CRUD = #crud{
       pagination_class=["btn", "btn-default"],
       button_class=["btn", "btn-link"],
       table_class=["table-striped", "table-bordered", "table-hover"],
       start=0,
       count=10,
       cols=[
             {id, "Name", tb},
             {title, "Title", ta},
             {description, "Description", ta},
             {module, "Module", {select, cms_modules()}},
             {accepted_role, "Assess role", {select, cms_roles()}}
            ],
       funs=#{
         list => fun db:get_pages/0,
         update => fun db:update_map/1, 
         delete => fun db:delete/1
        }
      },
    wf:update(container, [
                           #bs_row{
                              body=[
                                    #bs_col{
                                      cols={lg, 10},
                                      body=#h1{text="All Pages"}
                                              },
                                    #bs_col{
                                      cols={lg, 2},
                                      body=#button{
                                              text="Add Page",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({page, new})
                                             }}
                                   ]},
                           #bs_row{
                              body=#bs_col{
                                      cols={lg, 12},
                                      body=CRUD
                                     }
                             }]);
event({page, new}) -> % {{{2
    new_modal("Create Page", 
              {page, save},
              undefined,
              [
               #span{text="Name (ID)"},
               #txtbx{id=name,
                      placeholder="page_name"},

               #span{text="Page Title"},
               #txtbx{id=title,
                      placeholder="Page title here..."},

               #span{text="Description"},
               #txtarea{id=description,
                        placeholder="Description here..."},

               #span{text="Page Main Module"},
               #dd{
                  id=module,
                  value=index,
                  options=cms_modules()
                 },

               #span{text="Who Have Access"},
               #dd{
                  id=role,
                  value=undefined,
                  options=cms_roles()
                 }
              ]);

event({page, construct}) -> % {{{2
    Pages = db:get_pages(),
    [#{id := P} | _] = Pages,
    PID = common:q(page_select, P),
    Block = common:q(block_select, "page"),
    wf:wire(#event{postback={page, construct, PID, [Block]}});
    
event({page, construct, PID, [Block|_]=BlocksPath}) -> % {{{2
    Pages = db:get_pages(),
    Blocks = [format_block(B#cms_mfa{id={PID, BID}})
              || #cms_mfa{id={_, BID}}=B <- db:get_mfa(PID, Block)],
    AllBlocks = db:get_all_blocks(PID),
    PageSelect = [
                  #dropdown{
                     id=page_select,
                     options=[{N, N} || #{id := N} <- Pages],
                     value=PID,
                     postback={page, construct}
                    },
                  #span{text=" / "},
                  #dropdown{
                     id=block_select,
                     options=[{N, N} ||  N <- AllBlocks],
                     value=Block,
                     postback={page, construct}
                    }
                 ],
    Sort = #sortblock{
                  tag={PID, Block},
                  class="panel-body", %"page-block-sort",
                  items=Blocks,
                  group=blocks},

    Body  = #panel{
               class=["panel", "panel-default"],
               body=[
                     #panel{
                        class=["panel-heading"],
                        body=PageSelect
                       },
                     Sort
                    ]},

    update_container("Construct Page", "Add Block", {block, add}, Body);

event({page, save}) -> % {{{2
    PID = wf:q(name),
    Title = wf:q(title),
    Description = wf:q(description),
    Module = wf:to_atom(wf:q(module)),
    Role = wf:to_atom(wf:q(role)),
    add_page(PID, Title, Description, Role, Module),
    coldstrap:close_modal(),
    wf:wire(#event{postback={page, show}});
event({block, change, module}) -> % {{{2
    M = wf:to_atom(common:q(module, common)),
    wf:replace(function, 
               #dd{
                  id=function,
                  value=template,
                  postback={block, change, function},
                  options=apply(M, functions, [])
                 }),
    event({block, change, function});
event({block, change, function}) -> % {{{2
    M = wf:to_atom(common:q(module, common)),
    F = wf:to_atom(common:q(function, common)),
    BlockData = try apply(M, form_data, [F])
                catch error:undef -> {[], []}
                end,
    wf:update(block_data,
               render_fields(add_default_fields(BlockData)));
event({block, add}) -> % {{{2
    Pages = db:get_pages(),
    [#{id := P} | _] = Pages,
    PID = common:q(page_select, P),
    Block = common:q(block_select, "page"),
    Header = [
              "Add new block to page: ",
                  #dropdown{
                     id=add_page_select,
                     options=[{N, N} || #{id := N} <- Pages],
                     value=PID,
                     postback={page, construct}
                    },
                  " to block: ",
                  #textbox{
                     id=add_block,
                     text=Block
                     }
             ],
    new_modal(Header,
              {block, save},
              undefined, 
              [
               #span{text="Module"},
               #dd{id=module,
                  value=common,
                  postback={block, change, module},
                  options=modules()},

               #span{text="Block Type"},
               #dd{
                  id=function,
                  value=template,
                  postback={block, change, function},
                  options=common:functions()
                 },
               #panel{
                  id=block_data,
                  body=apply(common, form_data, [template])
                 }
              ]);

event({block, save}) -> % {{{2
    M = wf:to_atom(common:q(module, "common")),
    F = wf:to_atom(common:q(function, "template")),
    PID = common:q(add_page_select, "index"),
    Block = common:q(add_block, "body"),
    
    Rec = db:fix_sort(#cms_mfa{id={PID, Block}, 
                               mfa={M, F, []}}),
    NewRec = apply(M, save_block, [Rec]),
    db:save(NewRec),
    coldstrap:close_modal(),
    wf:wire(#event{postback={page, construct, PID, [Block]}});
event({block, remove, #cms_mfa{id={PID, Block}}=B}) -> % {{{2
    db:maybe_delete(B),
    wf:wire(#event{postback={page, construct, PID, [Block]}});
    
event(Ev) -> % {{{2
    wf:info("~p event ~p", [?MODULE, Ev]).

inplace_textbox_event({asset, Record, Field}, Value) -> % {{{2
    Val = db:update(Record, Field, Value),
    Val;
inplace_textbox_event(Tag, Value) -> % {{{2
    wf:info("~p inplace tb event ~p: ~p", [?MODULE, Tag, Value]),
    Value.
start_upload_event(_Tag) -> % {{{2
    ok.
finish_upload_event(template, Fname, Path, _Node) -> % {{{2
    NewPath = wf:f("templates/~s", [Fname]),
    file:rename(Path, NewPath),
    maybe_set(name, Fname),
    wf:set(path, NewPath);

finish_upload_event(asset, Fname, Path, _Node) -> % {{{2
    #cms_asset{type=Type} = file_to_asset(Fname, ""),
    URLPath = wf:f("~s/~s", [Type, Fname]),
    NewPath = wf:f("static/~s", [URLPath]),
    file:rename(Path, NewPath),
    maybe_set(name, Fname),
    wf:set(type, Type),
    wf:set(path, URLPath).

api_event(Name, Tag, Args) -> % {{{2
    wf:info("~p API event ~p(~p; ~p)", [?MODULE, Name, Tag, Args]).

sort_event({PID, Block}, Blocks) -> % {{{2
    wf:info("Blocks: ~p", [Blocks]),
    lists:foreach(fun({N, {block, PID, B}}) ->
                     db:update(B, B#cms_mfa{sort=N})
             end,
             lists:zip(lists:seq(1, length(Blocks)), Blocks)),
    wf:wire(#event{postback={page, construct, PID, [Block]}});
sort_event(SortTag, Blocks) -> % {{{2
    wf:warning("Wrong sort event in ~p tag: ~p Blocks: ~p", [?MODULE, SortTag, Blocks]).

%% Dropdown formatters {{{1
cms_modules() -> % {{{2
    [{index, "Main"},
     {admin, "Admin"},
     %{blog, "Blog"},
     {account, "Account"}
     %{galery, "Galery"}
     ].

cms_roles() -> % {{{2
    [{"undefined", "Nobody"},
     {admin, "Admin"},
     {root, "Root"},
     {editor, "Editor"}].

modules() -> % {{{2
    [
     {admin, "Admin"},
     {common, "Common"},
     {index, "Main"},
     {bootstrap, "Bootstrap"}
    ].

