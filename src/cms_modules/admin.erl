%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (admin).
-compile([export_all, {parse_transform, lager_transform}]).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("records.hrl").
-include("db.hrl").
-include("cms.hrl").

?DESCRIPTION(Admin).

%% Module install routines {{{1
default_data() -> % {{{2
    #{
  cms_page => [
               #cms_page{
                  id="admin",
                  module=index,
                  accepted_role=admin,
                  title= <<"Admin Page - LiquidCMS">>
                 }
              ],
  cms_mfa => [
              add_to_block("admin", "body", {template, "templates/internal/admin.html"}),

              add_to_block({"admin", "css"}, [
                                              {common, asset, [["css", "font-awesome"]]},
                                              {common, asset, [["css", "metisMenu"]]},
                                              {common, asset, [["css", "sb-admin-2"]]},
                                              {common, asset, [["css", "admin"]]},
                                              {common, asset, [["css", "bootstrap-treeview"]]}
                                             ]),

              add_to_block({"admin", "script"}, [
                                               {common, asset, [["js", "metisMenu"]]},
                                               {common, asset, [["js", "sb-admin-2"]]},
                                               {common, asset, [["js", "bootstrap-treeview"]]}
                                              ]),
                                                
              add_navbar_button("admin", "sidebar-nav", "assets", {{"fa", "hdd-o", []}, "Static Assets"}, {menu, "static-assets-menu"}),
              add_navbar_button("admin", "static-assets-menu", "assets-css", {{"fa", "css3", []}, "CSS"}, {event, ?POSTBACK({?MODULE, asset, show, css}, ?MODULE)}),
              add_navbar_button("admin", "static-assets-menu", "assets-scripst", {{"fa", "code", []}, "JavaScript"}, {event, ?POSTBACK({?MODULE, asset, show, script}, ?MODULE)}),
              add_navbar_button("admin", "static-assets-menu", "assets-img", {{"fa", "image", []}, "Images"}, {event, ?POSTBACK({?MODULE, asset, show, image}, ?MODULE)}),
              add_navbar_button("admin", "static-assets-menu", "assets-binary", {{"fa", "file-o", []}, "Other"}, {event, ?POSTBACK({?MODULE, asset, show, binary}, ?MODULE)}),

              add_navbar_button("admin", "sidebar-nav", "templates", {{"fa", "hdd-o", []}, "Templates"}, {event, ?POSTBACK({?MODULE, template, show}, ?MODULE)}),

              add_navbar_button("admin", "sidebar-nav", "pages", {{"fa", "file-o", []}, "Pages"}, {menu, "pages-menu"}),
              add_navbar_button("admin", "pages-menu", "pages-all", {{"fa", "file-o", []}, "All Pages"}, {event, ?POSTBACK({?MODULE, page, show}, ?MODULE)}),
              add_navbar_button("admin", "pages-menu", "page-construct", {{"fa", "puzzle-piece", []}, "Construct Page"}, {event, ?POSTBACK({?MODULE, page, construct}, ?MODULE)}),

              add_navbar_button("admin", "sidebar-nav", "accounts", {{"fa", "users", []}, "Access"}, {menu, "accounts-menu"}),
              add_navbar_button("admin", "accounts-menu", "users-all", {{"fa", "user", []}, "Users"}, {event, ?POSTBACK({?MODULE, user, show}, ?MODULE)}),
              add_navbar_button("admin", "accounts-menu", "roles-all", {{"fa", "group", []}, "Roles"}, {event, ?POSTBACK({?MODULE, role, show}, ?MODULE)}),

              add_navbar_button("admin", "sidebar-nav", "forms", {{"fa", "wpforms", ["fab"]}, "Forms"}, {event, ?POSTBACK({?MODULE, forms, show}, ?MODULE)}),
              add_navbar_button("admin", "sidebar-nav", "languages", {{"fa", "globe", ["fab"]}, "Languages"}, {event, ?POSTBACK({?MODULE, languages, show}, ?MODULE)})
             ]
 }.

install() -> % {{{2
    lager:info("Installing ~p module", [?MODULE]),
    get_files_from_folder("static"),
    get_files_from_folder("templates"),
    get_files_from_folder("templates/internal"),
    add_language("any","",false),
    add_language("ru","flag_ru.png",true),
    add_language("en","flag_en.png",false),
    ok.

%% Different components adding to pages  {{{1
add_page(PID, Title, Description, Role, Module, Sitemap) -> % {{{2
    #cms_page{
       id=PID,
       title=wf:f("~ts", [Title]),
       description=wf:f("~ts", [Description]),
       accepted_role=Role,
       module=Module,
       sitemap=Sitemap
      }.

add_form(PID, Phone, Text, Email, Rating) -> % {{{2
    db:save(#cms_form{
        id=crypto:hash(sha512, Email++Text),
        page=PID,
        phone=Phone,
        text=wf:f("~ts", [Text]),
        email=Email,
        rating=Rating
    }).

add_language(Lid, ImageName, IsDefault) -> % {{{2
    mnesia:transaction(
      fun() ->
        mnesia:write(#cms_language{id=Lid, icon=ImageName, default=IsDefault})
      end).

add_template(TemplatePath, Bindings) -> % {{{2
    add_template(TemplatePath, TemplatePath, TemplatePath, Bindings).

add_template(Name, Description, TemplatePath, Bindings) -> % {{{2
    IsTemplate = filelib:is_regular(TemplatePath),
    if IsTemplate ->
           db:save(#cms_template{
                      file = TemplatePath,
                      name = Name,
                      description = Description,
                      bindings = Bindings
                     });
       true ->
           {error, no_template}
    end.

add_to_block({PID, Block}, Mater) when is_list(Mater)  -> % {{{2
    lists:flatten([add_to_block({PID, Block}, M) || M <- Mater]);
add_to_block({PID, Block}, {_M, _F, _A}=Mater) -> % {{{2
    add_to_block(PID, Block, Mater);
add_to_block(Id, {{_M, _F, _A}=MFA, Settings}) -> % {{{2
    #cms_mfa{
       id=Id,
       mfa=MFA,
       settings=Settings
      }.

add_to_block(PID, Block, Mater)  -> % {{{2
    add_to_block(PID, Block, Mater, 1).

add_to_block(PID, Block, {Type, ID}, Sort) -> % {{{2
    add_to_block(PID, Block, {common, Type, [ID]}, Sort);
add_to_block(PID, Block, {M, F, A}, Sort) -> % {{{2
    #cms_mfa{
       id={PID, Block},
       mfa={M, F, A},
       sort=Sort}.

add_navbar_button(PID, MenuBlock, ItemBlock, {Icon, Text}, {menu, SubMenuBlock}) -> % {{{2
    ItemLinkBlock = common:sub_block(ItemBlock, "link"),
    ButtonMFAs = [
                  #cms_mfa{id={PID, MenuBlock},
                           mfa={html5,
                                list_item,
                                [ItemBlock]}},
                  #cms_mfa{id={PID, "body"},
                           mfa={common, script, [wf:f("$('#~s').metisMenu();", [MenuBlock])]}},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={html5,
                                link_url,
                                [ItemLinkBlock, ""]}},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={bootstrap,
                                nav_items,
                                [SubMenuBlock, ["nav-second-level", "collapse","dropdown-menu"]]}}
                 ],
    LinkMFAs = [
                #cms_mfa{
                   id={PID, ItemLinkBlock},
                   mfa={common, icon, ["fa", "", ["arrow-right"]]}}
                | link_body_funs(PID, ItemLinkBlock, Icon, Text)],
    db:fix_sort(ButtonMFAs ++ LinkMFAs);
add_navbar_button(PID, MenuBlock, ItemBlock, {Icon, Text}, {event, Actions}) -> % {{{2
    ItemLinkBlock = common:sub_block(ItemBlock, "link"),
    ButtonMFAs = [
                  #cms_mfa{id={PID, MenuBlock},
                           mfa={html5,
                                list_item,
                                [ItemBlock]}},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={html5,
                                link_event,
                                [ItemLinkBlock, Actions]}}
                 ],
    LinkMFAs = link_body_funs(PID, ItemLinkBlock, Icon, Text),
    db:fix_sort(ButtonMFAs ++ LinkMFAs);
add_navbar_button(PID, MenuBlock, ItemBlock, {Icon, Text}, {url, URL}) -> % {{{2
    ItemLinkBlock = common:sub_block(ItemBlock, "link"),
    ButtonMFAs = [
                  #cms_mfa{id={PID, MenuBlock},
                           mfa={html5,
                                list_item,
                                [ItemBlock]}},
                  #cms_mfa{id={PID, ItemBlock},
                           mfa={html5,
                                link_url,
                                [ItemLinkBlock, URL]}}
                 ],
    LinkMFAs = link_body_funs(PID, ItemLinkBlock, Icon, Text),
    db:fix_sort(ButtonMFAs ++ LinkMFAs).

%% Helpers and private functions {{{1
menu_item_funs(Page, MenuBlock, ItemSub, URL) -> % {{{2
    ItemBlock = common:sub_block(MenuBlock, ItemSub),
    ItemBodyBlock = common:sub_block(ItemBlock, "body"),

    [
     #cms_mfa{id={Page, MenuBlock},
              mfa={html5,
                   list_item,
                   [ItemBlock]},
              sort=1},
     #cms_mfa{id={Page, ItemBlock},
              mfa={html5,
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
                      mfa={common, icon, A}};
         _ ->
             []
     end,
     if Text /= undefined ->
            #cms_mfa{id={PID, LinkBlock},
                     mfa={common, text, [" " ++ Text]}};
        true -> []
     end
    ].

file_to_asset(File, Path) -> % {{{2
    [Ext, Min | Id] = lists:reverse(string:tokens(File, ".")),
    case {string:to_lower(Ext), string:to_lower(Min)} of
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
        {Any, _} when Any == "jpg";
                      Any == "jpeg";
                      Any == "png";
                      Any == "gif";
                      Any == "svg" ->
            #cms_asset{
               id=[Ext, Min | Id],
               name=string:join(lists:reverse([Min|Id]), "."),
               description=string:join(lists:reverse([Min|Id]), "."),
               file=filename:join([Path, File]),
               type=image};

        {_Any, _} ->  % This is any static files except images, js and css
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
            ?LOG("~s value is ~p", [Id, A]),
            ""
    end.

update_container(Header, ButtonText, ButtonPostBack, Body) -> % {{{2
    wf:update(container, [
                          #bs_row{
                             body=[
                                   #bs_col{
                                      cols=[{lg, 2}, {md, 3},{sm, 3}],
                                      body=#button{
                                              text=ButtonText,
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK(ButtonPostBack, ?MODULE)
                                            }
                                    }
                                  ]
                          },
                          #bs_row{
                             body=[#bs_col{
                                     cols=[{lg, 6}, {md, 6},{sm, 6}],
                                     body=Body
                                    },
                                   #bs_col{
                                     cols=[{lg, 6}, {md, 6},{sm, 6}],
                                     id=edited_block
                                    }]
                            }]),
    wf:update(title, #panel{text=Header}).

render_save_button({SavePostback, Delegate}) when is_tuple(SavePostback), % {{{2
                                                  is_atom(Delegate) ->
    #btn{
       id=save_button,
       type=success,
       size=md,
       text="Save",
       postback=SavePostback,
       delegate=Delegate
      };
render_save_button(SavePostback) -> % {{{2
    render_save_button({SavePostback, ?MODULE}).

render_ok_button({OkPostback, Delegate}) when is_tuple(OkPostback), % {{{2
                                              is_atom(Delegate) ->
    #btn{
       id=ok_button,
       type=success,
       size=md,
       text="Ok",
       postback=OkPostback,
       delegate=Delegate
      };
render_ok_button(OkPostback) -> % {{{2
    render_ok_button({OkPostback, ?MODULE}).

render_cancel_button() -> % {{{2
    #btn{
       id=cancel_button,
       type=warning,
       size=md,
       text="Cancel",
       postback=close_modal,
       delegate=?MODULE
      }.

render_copy_button(undefined) -> % {{{2
    "";
render_copy_button(CopyPostback) -> % {{{2
    #btn{
       type=warning,
       size=md,
       text="Copy",
       postback=CopyPostback,
       delegate=?MODULE
      }.

render_move_button(MovePostback) -> % {{{2
    #btn{
       id=save_button,
       type=success,
       size=md,
       text="Move",
       postback=MovePostback,
       delegate=?MODULE
      }.

new_modal(Title, OkPostback, Form) -> % {{{2
    Bottom = #span{
            id=modal_bottom_button,
            body=[
                  render_ok_button(OkPostback),
                  render_cancel_button()
                 ]},
    Body = #bs_row{
            body=[
                #bs_col{
                   body=Form}
               ]},
    coldstrap:modal(Title, Body, Bottom, [{has_x_button, true}]).

new_modal(Title, SavePostback, UploadTag, Form) -> % {{{2
    new_modal(Title, SavePostback, undefined, UploadTag, Form).

new_modal(Title, SavePostback, CopyPostback, UploadTag, Form) -> % {{{2
    Bottom = #span{
                id=modal_bottom_buttons,
                body=[
                      render_copy_button(CopyPostback),
                      render_save_button(SavePostback)
                     ]},

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
                               delegate=?MODULE,
                               show_button=false,
                               overall_progress=true
                              }},
                    #bs_col{
                       cols=BodyCols,
                       body=Form}
                   ]},
    coldstrap:modal(Title, Body, Bottom, [{has_x_button, true}]).

format_block(#cms_mfa{ % {{{2$
                id={PID, Name},
                mfa={M, F, A}
               }=B) ->
    {Body, Sub} = try apply(M, format_block, [F, A])
                  catch
                      _:_ -> {wf:f("~p, ~p(~p)", [Name, F, A]), undefined}
                  end,
    Children=db:get_mfa(PID,Sub),
    #sortitem{
       tag={block, PID, B},
       class="well",
       body=[
             Body,
             #span{
                class="pull-right",
                style="margin-top:-7px",
                body=[
                      #link{
                         class="btn btn-link",
                         body=common:icon("fa", "plus", []),
                         show_if=(Sub /= undefined),
                         actions=?POSTBACK({?MODULE, block, add, Sub}, ?MODULE)
                        },
                      #link{
                         class="btn btn-link",
                         body=common:icon("fa", "arrow-right", []),
                         show_if=((Sub /= undefined) and (length(Children)>0)),
                         actions=?POSTBACK({?MODULE, page, construct, PID, [Sub]}, ?MODULE)
                        },
                      #link{
                         class="btn btn-link",
                         body=common:icon("fa", "pencil", []),
                         actions=?POSTBACK({?MODULE, block, edit, B}, ?MODULE)
                        },
                      #link{
                         class="btn btn-link",
                         body=common:icon("fa", "remove", []),
                         actions=#event{
                                    type=click,
                                    actions=#confirm{
                                               text="Are you sure to delete?", 
                                               postback={?MODULE, block, remove_block, B},
                                               delegate=?MODULE}}
                        }
                     ]}
            ]}.

format_subblock(#cms_mfa{ id={PID, Name}}=B) -> % {{{2
    #sortitem{
       tag={block, PID, B},
       % class="well",
       body=[
             #span{
                class="panel-format-btn",
                body=[
                      #link{
                         class="btn btn-link",
                         body=common:icon("fa", "arrow-right", []),
                         % show_if=(Sub /= undefined),
                         actions=?POSTBACK({?MODULE, page, close_and_construct, PID, [Name]}, ?MODULE)
                        }
                     ]}
            ]}.

add_default_fields({Data, Formatting}) -> % {{{2
    add_default_fields(Data, Formatting, "", "", "");
add_default_fields({Data, Formatting, Block, Classes}) -> % {{{2
    add_default_fields(Data, Formatting, Block, Classes, "");
add_default_fields({Data, Formatting, Block, Classes, DataAttrs}) -> % {{{2
    add_default_fields(Data, Formatting, Block, Classes, DataAttrs);
add_default_fields(Any) when is_list(Any) -> % {{{2
    [{"", Any}, {"", []}];
add_default_fields(Any) -> % {{{2
    [{"", Any}, {"", []}].

add_default_fields(Data, Formatting, Block, Classes, DataAttrs) -> % {{{2
    [ 
     {"Data", [
               {"Block name", {block, Block}}
               | Data]},
     {"Formatting",
      Formatting ++ [{"Additional classes", {classes, Classes}}]
       ++ [{"Data-attributes", {data_fields, format_data_attrs(DataAttrs)}}]
     }
    ].

form_fields(M, F, A) -> % {{{2
    try 
        apply(M, form_data, [F, A])
    catch error:E when E /= undef; 
                       E /= function_clause -> 
              % ?LOG("Error in form_fields:~p",[E]),
              [_, Block, Classes, DataAttrs] = maybe_empty(A, 4),
              {[], [], Block, Classes, DataAttrs}
    end.

form_elements(M, F, A) -> % {{{2
    render_fields(
      add_default_fields(
        form_fields(M, F, A))).

render_fields(Cols) -> % {{{2
    try 12 div length(Cols) of
        Width when Width >= 4 ->
            [
             #bs_row{
                body=[render_field(Col, Width) || Col <- Cols]
               }];
        _Width ->
            {Row, Rows} = lists:split(3, Cols),
            [render_fields(Row) | render_fields(Rows)]

    catch 
        _:_ -> 
            [
             #bs_row{
                body=[render_field(Col, 12) || Col <- Cols]
               }]
    end.

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
                text=Text,
                rows=1
               }
            ]};
render_field({Label, Any}, _Width) when is_list(Any) -> % {{{2
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


get_with_prefix(M, Prefix, Label) -> % {{{2
    Fields = formatting_fields(form_fields(M, Prefix, [])),
    AllData = wf:mq([Label | Fields]),
    lists:map(fun(none) -> "";
                 (undefined) -> "";
                 ("") -> "";
                 (Attr) -> Attr
              end,
              AllData).

extract_data_attrs([])  -> % {{{2
    [];
extract_data_attrs(DataAttrs)  -> % {{{2
    Pairs = string:tokens(DataAttrs, ", "),
    [{data_attr_key(K), V} || Pair <- Pairs, [K, V] <- [string:tokens(Pair, "=")]].

data_attr_key([$d, $a, $t, $a, $- | K]) -> % {{{2
    data_attr_key(K);
data_attr_key(K) -> % {{{2
    wf:to_atom(K).

format_data_attrs(Attrs) -> % {{{2
    [[K, "=", V, " "] || {K, V} <- Attrs].

get_data(M, F) -> % {{{2
    Fields = data_fields(form_fields(M, F, [])),
    Data = case common:q(copy_block_name, undefined) of 
      undefined->
        wf:mq([block | Fields]);
      _ ->
        wf:mq([copy_block_name | Fields])
    end,
    lists:map(fun(none) -> "";
                 (undefined) -> "";
                 ("") -> "";
                 (Field) -> Field
              end,
              Data).

data_fields({Data, _, _, _}) -> % {{{2
    data_fields(Data);
data_fields({Data, _, _, _,_}) -> % {{{2
    data_fields(Data);
data_fields({Data, _}) -> % {{{2
    data_fields(Data);
data_fields(Data) when is_list(Data) -> % {{{2
    lists:flatten([get_fields(F) || F <- Data, get_fields(F) /= []]).

formatting_fields({_, Formats, _, _}) -> % {{{2
    [get_fields(F) || F <- Formats, get_fields(F) /= []];
formatting_fields({_, Formats}) -> % {{{2
    [get_fields(F) || F <- Formats, get_fields(F) /= []];
formatting_fields(_Any) -> % {{{2
    [].

get_fields({_, {ID, _}}) when is_atom(ID) -> % {{{2
    ID;
get_fields({_, ID}) when is_atom(ID) -> % {{{2
    ID;
get_fields({_, Any}) -> % {{{2
    get_fields(Any);
get_fields(#dd{id=ID}) -> % {{{2
    ID;
get_fields(#txtbx{id=ID}) -> % {{{2
    ID;
get_fields(#txtarea{id=ID}) -> % {{{2
    ID;
get_fields(#panel{body=Body}) when is_list(Body) -> % {{{2
    [get_fields(E) || E <- Body];
get_fields(#panel{body=Body}) -> % {{{2
    get_fields(Body);
get_fields(_D) -> % {{{2
    % ?LOG("Data: ~p", [D]),
    [].

prefix_classes(Prefix, Classes) -> % {{{2
    [wf:f("~s-~s", [Prefix, C]) || C <- Classes].

remove_prefix([]) -> % {{{2
    "";
remove_prefix(Class) -> % {{{2
    lists:last(string:tokens(Class, "-")).

maybe_empty([], N) -> % {{{2
    lists:duplicate(N, "");
maybe_empty(A, N) when length(A) < N -> % {{{2
    Delta = N - length(A),
    A ++ lists:duplicate(Delta, "");
maybe_empty(A, _N) -> % {{{2
    A.

get_files_from_folder("static"=SubFolder) -> % {{{2
    %{ok, StaticFolders} = application:get_env(simple_bridge, static_paths),
    {ok, StaticFolders} = file:list_dir(SubFolder),
    {ok, OldCWD} = file:get_cwd(),
    file:set_cwd(SubFolder),
    Static = [{wf:f("~s", [Path]), Files} ||
              Path <- StaticFolders,
              filelib:is_dir(Path),
              {ok, Files} <- [file:list_dir(Path)],
              Path /= "nitrogen"],
    Assets = lists:foldl(fun({Path, Files}, A) ->
                                 [file_to_asset(File, Path) || File <- Files, File /= ".empty"] ++ A
                         end,
                         [],
                         Static),
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
    db:maybe_save(Assets ++ NitrogenAssets),
    file:set_cwd(OldCWD);

get_files_from_folder(SubFolder) -> % {{{2
    {ok, TemplateFiles} = file:list_dir(SubFolder),
    lists:foreach(fun(F) -> add_template(wf:f("~s/~s", [SubFolder, F]), []) end, TemplateFiles).

get_social_files("static/images"=SubFolder) -> % {{{2
    {ok, StaticImages} = file:list_dir(SubFolder),
    Static = [{"images", StaticImages}],
    Assets = lists:foldl(fun({Path, Files}, A) ->
                                 [file_to_asset(File, Path) || File <- Files,
                                  string:str(File, "_icon.png") > 0 ] ++ A
                         end,
                         [],
                         Static),
    db:save(Assets).

get_filters(#cms_mfa{settings=#{filters := Filters}}) -> % {{{2
    Filters;
get_filters(_) -> % {{{2
    ["", "", "", ""].

maybe_fix_sort(#cms_mfa{sort=new}=R, _) -> % {{{2
    db:fix_sort(R);
maybe_fix_sort(#cms_mfa{id=Id}=R, #cms_mfa{id=Id}) -> % {{{2
    R;
maybe_fix_sort(R, _) -> % {{{2
    db:fix_sort(R).

fix_list_sort(List) -> % {{{2
    Sorted = lists:keysort(#cms_mfa.id, 
                           lists:flatten(List)),
    lists:foldl(fun(#cms_mfa{id={P, B}}=MFA, [#cms_mfa{id={P, B}, sort=S}|_]=A) ->
                        [MFA#cms_mfa{sort=S + 1} | A];
                   (MFA, A) ->
                        [MFA | A]
                end,
               [],
               Sorted).

rec_from_qs(R) -> % {{{2
    M = wf:to_atom(common:q(module, "common")),
    F = wf:to_atom(common:q(function, "text")),
    Args = admin:get_data(M, F),
    PID = common:q(add_page_select, "index"),
    Block = common:q(add_block, "body"),

    Classes = get_with_prefix(M, wf:to_atom(F), classes),
    DataAttrs =extract_data_attrs(common:q(data_fields, "")),

    Filters=wf:mq([qs_key, qs_val, role, translation]),
    Settings = case Filters of
      [undefined,undefined,undefined,undefined] -> R#cms_mfa.settings;
      _ -> #{filters => Filters}
    end,
    case F of 
      empty -> R#cms_mfa{mfa=undefined};
      _ ->
        R#cms_mfa{id={PID, Block}, 
              mfa={M, F, Args ++ [Classes] ++ [DataAttrs]},
              settings=Settings}
    end.

apply_element_transform(#cms_mfa{mfa=MFA}=Rec) -> % {{{2
    {M,_F,_A}=parse_mfa_field(MFA),
    try apply(M, save_block, [Rec])
    catch 
        error:undef -> Rec;
        error:function_clause -> Rec 
    end.
get_pages() -> % {{{2
    [#{id => "*"} | db:get_pages()].

%% Event handlers {{{1
event({common, edit, text, #cms_mfa{id={PID, Block}, sort=S}=MFA, Text}) -> % {{{2
    {atomic, UpdText}=mnesia:transaction(
        fun() ->
          case mnesia:match_object(#cms_mfa{id={PID, Block}, sort=S, _='_'}) of
            L when is_list(L), length(L) > 0 ->
              [ Itext || #cms_mfa{mfa={common,text, [Itext]}} <-L];
            _ -> Text
          end
        end),
    new_modal("Edit text", 
              {?MODULE, block, move, MFA},
              undefined,
              [
               #hidden{id=module, text=common},
               #hidden{id=function, text=text},
               #hidden{id=add_page_select, text=PID},
               #hidden{id=add_block, text=Block},
               #wysiwyg{class=["form-control"],
                        id=text_mfa,
                        html=UpdText,
                        buttons=[
                                 #panel{
                                    class="btn-group",
                                    body=[
                                          #wysiwyg_button{
                                             body="B",
                                             func="bold",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i>I</i>",
                                             func="italic",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<u>U</u>",
                                             func="underline",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<s>S</s>",
                                             func="strikethrough",
                                             class=["btn", "btn-default"]}
                                         ]},
                                 #panel{
                                    class="btn-group",
                                    body=[
                                          #wysiwyg_button{
                                             body="<i class='fa fa-align-left'></i>",
                                             func="justifyleft",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i class='fa fa-align-center'></i>",
                                             func="justifycenter",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i class='fa fa-align-right'></i>",
                                             func="justifyright",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i class='fa fa-align-justify'></i>",
                                             func="justifyfull",
                                             class=["btn", "btn-default"]}
                                         ]},
                                 #panel{
                                    class="btn-group",
                                    body=[
                                          #wysiwyg_button{
                                             body="<i class='fa fa-undo'></i>",
                                             func="undo",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i class='fa fa-repeat'></i>",
                                             func="redo",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i class='fa fa-close'></i>",
                                             func="removeFormat",
                                             class=["btn", "btn-default"]}
                                         ]},
                                 #panel{
                                    class="btn-group",
                                    body=[
                                          #wysiwyg_button{
                                             body="<i class='fa fa-list-ol'></i>",
                                             func="insertorderedlist",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i class='fa fa-list-ul'></i>",
                                             func="insertunorderedlist",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i class='fa fa-outdent'></i>",
                                             func="outdent",
                                             class=["btn", "btn-default"]},
                                          #wysiwyg_button{
                                             body="<i class='fa fa-indent'></i>",
                                             func="indent",
                                             class=["btn", "btn-default"]}
                                         ]}
                                ]}
              ]);

event({?MODULE, asset, new}) -> % {{{2 TODO: Check and remove _Type at all
    new_modal("Upload Static Asset",
              {?MODULE, asset, save},
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
event({?MODULE, asset, refresh, Type}) -> % {{{2
    get_files_from_folder("static"),
    event({?MODULE, asset, show, Type});
event({?MODULE, asset, save}) -> % {{{2
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
    wf:wire(#event{postback={?MODULE, asset, show, Type}, delegate=?MODULE});
event({?MODULE, asset, show, Type}) -> % {{{2
    wf:session(history, undefined),
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
                                      cols=[{lg, 2},{md, 3}, {sm, 3}],
                                      body=#button{
                                              text="Refresh filesystem",
                                              class=["btn",
                                                     "btn-warning",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({?MODULE, asset, refresh, Type}, ?MODULE)
                                             }},
                                   #bs_col{
                                      cols=[{lg, 2},{md, 3}, {sm, 3}],
                                      body=#button{
                                              text="Upload Asset",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({?MODULE, asset, new}, ?MODULE)
                                             }}
                                  ]},
                          #bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]),
    wf:update(title, #panel{text="Static Assets"});
event({?MODULE, template, new}) -> % {{{2
    new_modal("Upload Template",
              {?MODULE, template, save},
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

event({?MODULE, template, refresh}) -> % {{{2
    get_files_from_folder("templates"),
    event({template, show});
event({?MODULE, template, save}) -> % {{{2
    Path = wf:q(path),
    add_template(wf:q(name), wf:q(description), Path, []),
    coldstrap:close_modal(),
    wf:wire(#event{postback={template, show}});
event({?MODULE, template, show}) -> % {{{2
    wf:session(history, undefined),
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
                                      cols=[{lg, 2},{md, 3}, {sm, 3}],
                                      body=#button{
                                              text="Refresh filesystem",
                                              class=["btn",
                                                     "btn-warning",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({?MODULE, template, refresh}, ?MODULE)
                                             }},
                                   #bs_col{
                                      cols=[{lg, 2},{md, 3}, {sm, 3}],
                                      body=#button{
                                              text="Upload Template",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({?MODULE, template, new}, ?MODULE)
                                             }}
                                  ]},
                          #bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]),
    wf:update(title, #panel{text="Templates"});
event({?MODULE, forms, show}) -> % {{{2
    wf:session(history, undefined),
    CRUD = #crud{
              pagination_class=["btn", "btn-default"],
              button_class=["btn", "btn-link"],
              table_class=["table-striped", "table-bordered", "table-hover"],
              start=0,
              count=10,
              cols=[
                    {page, "Page", none},
                    {phone, "Phone", none},
                    {text, "Text", none},
                    {email, "Email", none},
                    {created_at, "Date", none}, %fun(Field, {{Y, M, D}, _}) -> ... end}
                    {rating, "Rating", none},
                    {comment, "Comment", ta}
                   ],
              funs=#{
                list => fun db:get_forms/0,
                update => fun db:update_map/1, 
                delete => fun db:delete/1
               }
             },
    wf:update(container, [#bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]),
    wf:update(title, #panel{text="Forms"});
event({?MODULE, languages, show}) -> % {{{2
    wf:session(history, undefined),
    AssetsDup = db:get_assets(image),
    GetName = fun(Id)->
                string:join(lists:reverse(Id),".")
              end, 
    Options =  [
      {GetName(maps:get(id, K)), GetName(maps:get(id, K))}
        || K <- AssetsDup
    ],
    CRUD = #crud{
              pagination_class=["btn", "btn-default"],
              button_class=["btn", "btn-link"],
              table_class=["table-striped", "table-bordered", "table-hover"],
              start=0,
              count=10,
              cols=[
                    {id, "Id", none},
                    {icon, "Icon", {select, Options}},
                    {default, "Is default", {select, [{true,"true"},{false,"false"}]}}
                   ],
              funs=#{
                list => fun db:get_languages/0,
                update => fun db:update_language/1, 
                delete => fun db:full_delete/1
               }
             },
    wf:update(container, [
                          #bs_row{
                             body=[#bs_col{
                                      cols=[{lg, 2},{md, 3}, {sm, 3}],
                                      body=#button{
                                              text="Add language",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({?MODULE, language, new}, ?MODULE)
                                             }}
                                  ]},
                          #bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]),
    wf:update(title, #panel{text="All languages"});
event({?MODULE, page, show}) -> % {{{2
    wf:session(history, undefined),
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
                    {module, "Module", {select, modules()}},
                    {accepted_role, "Assess role", {select, cms_roles()}},
                    {sitemap, "Sitemap", {select, sitemap:sitemap_frequencies()}},
                    {undefined, "Actions", button}
                   ],
              funs=#{
                list => fun db:get_pages/0,
                update => fun db:rename_page/1, 
                delete => fun db:delete/1,
                copy_page => fun db:copy_page/1
               }
             },
    wf:update(container, [
                          #bs_row{
                             body=[#bs_col{
                                      cols=[{lg, 2},{md, 3}, {sm, 3}],
                                      body=#button{
                                              text="Add Page",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({?MODULE, page, new}, ?MODULE)
                                             }}
                                  ]},
                          #bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]),
    wf:update(title, #panel{text="All Pages"});
event({?MODULE, page, new}) -> % {{{2
    new_modal("Create Page", 
              {?MODULE, page, save},
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
                  options=modules()
                 },

               #span{text="Who Have Access"},
               #dd{
                  id=role,
                  value=undefined,
                  options=cms_roles()
                 },

                #span{text="Sitemap frequency"},
               #dd{
                  id=sitemap,
                  value=none,
                  options=sitemap:sitemap_frequencies()
                 }
              ]);
event({?MODULE, page, construct}) -> % {{{2
    Pages = get_pages(),
    [#{id := P} | _] = Pages,
    PID = common:q(page_select, P), 
    {_, BlockId} = get_selected_block(),
    common:script("","$('#tree').html('<div>&nbsp;loading...</div>'); "),
    wf:wire(#event{postback={?MODULE, page, construct, PID, [BlockId]}, delegate=?MODULE});

event({?MODULE, page, construct, PID, [_Block|_]}) -> % {{{2
    Pages = get_pages(),
    % Blocks = [format_block(B#cms_mfa{id={PID, BID}})
    %           || #cms_mfa{id={_, BID}}=B <- db:get_mfa(PID, Block)],
    % _AllBlocks = db:get_all_blocks(PID),
    % ShowAll = (common:q(show_all, "false") /= "false"),

    % Session=case Blocks of
    %   [] ->  wf:clear_session(),
    %          [];
    %   _ ->  
    %     update_session_history(Block)
    % end,
    % _ViewSession = [
    %   #button{
    %         text=B,
    %         class=["btn btn-link session-btn"],
    %         actions=?POSTBACK({?MODULE, page, construct, PID, [B]}, ?MODULE)
    %   } || B <- Session],
    PageSelect = [
              #panel{id=left_manage_panel,
                    class="col-md-6",
                    style="width:auto;",
                    body=[
                      #span{text="Page: "},
                      #dropdown{
                         id=page_select,
                         options=lists:keysort(1,  [{N, N} || #{id := N} <- Pages]),
                         value=PID,
                         delegate=?MODULE,
                         postback={?MODULE, page, construct}
                        }
                      % ,#span{text="Show All ", class="cs-label"},
                      % #checkbox{
                      %    text="",
                      %    id=show_all,
                      %    checked=ShowAll,
                      %    delegate=?MODULE,
                      %    postback={?MODULE, page, construct}
                      %   }
                      % ,case ViewSession of
                      %   [] -> [];
                      %   _ ->
                      %     [#span{
                      %      text="History:" ,
                      %      class="cs-label"
                      %     },
                      %     ViewSession]
                      % end
                      ]
                }
    ],
    % Sort = #sortblock{
    %           tag={PID, Block},
    %           class="panel-body", %"page-block-sort",
    %           delegate=?MODULE,
    %           items=Blocks,
    %           group=blocks},

     
    Tree=build_html_tree_from_mfa(PID),

    Body  = #panel{
               class=["panel", "panel-default"],
               body=[
                     #panel{
                        class=["panel-heading flex-panel"],
                        body=PageSelect
                       },
                     #panel{
                        body=Tree,
                        html_id="tree"
                      }
                     % Sort
                    ]},
    
    
    update_container("Construct Page", "Add Block", {?MODULE, block, add}, Body),
    common:script("","$('#tree').treed(); $('#tree').open_last_active();")
    ;

event({?MODULE, page, save}) -> % {{{2 onclick <Save> btn
    PID = wf:q(name),
    Title = wf:q(title),
    Description = wf:q(description),
    Module = wf:to_atom(wf:q(module)),
    Role = wf:to_atom(wf:q(role)),
    Sitemap = wf:to_atom(wf:q(sitemap)),
    db:save(
      add_page(PID, Title, Description, Role, Module, Sitemap)),
    coldstrap:close_modal(),
    wf:wire(#event{postback={?MODULE, page, show}, delegate=?MODULE});
event({?MODULE, block, change, module}) -> % {{{2
    M = wf:to_atom(common:q(module, common)),
    wf:replace(function, 
               #dd{
                  id=function,
                  value=template,
                  postback={?MODULE, block, change, function},
                  delegate=?MODULE,
                  options=lists:keysort(2, apply(M, functions, []))
                 }),
    wf:wire(#event{postback={?MODULE, block, change, function}, delegate=?MODULE});
event({?MODULE, block, change, function}) -> % {{{2
    M = wf:to_atom(common:q(module, common)),
    F = wf:to_atom(common:q(function, common)),
    wf:update(block_data, admin:form_elements(M, F, []));
event({?MODULE, block, add}) -> % {{{2
    PID = common:q(page_select, "index"),
    {SelectedBlock, BlockId} = get_selected_block(),
    B = #cms_mfa{
           id={PID, BlockId},
           mfa={common, text, []},
           sort=new},
    case check_if_block_can_has_subblocks(SelectedBlock) of
      true->
        event({?MODULE, block, edit, B});
      false -> wf:update(edited_block, #panel{class="collapse"})
    end,
    wf:session(selected_block, {BlockId, []})
    ;
event({?MODULE, block, add, Block}) -> % {{{2
    PID = common:q(page_select, "index"),
    B = #cms_mfa{
           id={PID, Block},
           mfa={common, text, []},
           sort=new},
    event({?MODULE, block, edit, B});
event({?MODULE, block, edit, #cms_mfa{id={PID_block, Block}, mfa=MFA, sort=S}=B}) -> % {{{2
    {M,F,A}=parse_mfa_field(MFA), 
    PID = common:q(page_select, "index"),
    PagedBlock=B#cms_mfa{id={PID, Block}},
    Pages = get_pages(),
    [#{id := _P} | _] = Pages,
    [QSKey, QSVal, Role, Tr] = get_filters(B),
    Translation = case Tr of
      undefined -> any;
      [] -> any;
      Tr -> Tr
    end,
    LanguageOptions = [ {I, I} || #{id := I} <- db:get_languages()],
    Header = [
              #span{text="Add block to page: "},
              #dropdown{
                 id=add_page_select,
                 options=[{N, N} || #{id := N} <- Pages],
                 value=PID
                },
              " to block: ",
              #textbox{
                 id=add_block,
                 text=Block,
                 style="margin-bottom:1px; min-width: 200px;"
                },
              if (PID /= PID_block) and (PID_block=="*") -> 
                #span{text=" inherited from * ", style="color: cornflowerblue;"};
                true -> undefined
              end
             ],
    % new_modal(Title, SavePostback, CopyPostback, UploadTag, Form),

    Form=[
     Header,
     #dd{id=module,
         value=M,
         postback={?MODULE, block, change, module},
        delegate=?MODULE,
         options=collections()},

     #span{text="Block Type"},
     #dd{
        id=function,
        value=F,
        postback={?MODULE, block, change, function},
        delegate=?MODULE,
        options=(M):functions()
       },

     #panel{
        id=block_data,
        body=form_elements(M, F, [PID | A])
       },
     #span{text="Sort"},
     #txtbx{id=sort, text=S},
     #panel{
        body=render_fields([{"Filters", 
                             [
                              {"Query String", 
                               [
                                {"Key", {qs_key, QSKey}},
                                {"Value", {qs_val, QSVal}}
                               ]},
                              {"Role", {role, Role}},
                              {"Translation", #dd{
                                id=translation,
                                value=Translation,
                                options=LanguageOptions
                              }}
                             ]}])
               }
    ],
    Bottom = #panel{
      id=modal_bottom_buttons,
      body=[
            render_save_button({?MODULE, block, move, PagedBlock}),
            render_copy_button({?MODULE, block, copy, B}),
            #btn{
                   type=warning,
                   size=md,
                   text="Copy Structure",
                   postback={?MODULE, block, maybe_copy_all_structure, B},
                   delegate=?MODULE
                  },
            #btn{
                   type=info,
                   size=md,
                   text="Add Subblock",
                   postback={?MODULE, block, add},
                   delegate=?MODULE
                  },
            #btn{
                   type=danger,
                   size=md,
                   class="pull-right",
                   text="Remove", %common:icon("fa", "remove", []),
                   actions=#event{
                              type=click,
                              actions=#confirm{
                                         text="Are you sure to delete?", 
                                         postback={?MODULE, block, remove_block, PagedBlock},
                                         delegate=?MODULE}}
                  }
           ]
    },

    wf:update(edited_block,
        #panel{
              body=[
                    #bs_col{
                       body=Form},
                    Bottom
                   ]}
    );

event({?MODULE, language, new}) -> % {{{2
    new_modal("Add language", 
              {?MODULE, language, save},
              undefined,
              [
               #span{text="Name (ID)"},
               #txtbx{id=id,
                      placeholder="language_abbr"},

               #span{id=icon,text="Icon"},
               common:assets_dropdown(image),

               #span{text="Default"},
               #checkbox{id=default_language}
              ]);
event({?MODULE, language, save}) -> % {{{2    
    AssetId=wf:q(asset_id),
    IconName=string:join(lists:reverse(string:tokens(AssetId,".")),"."),
    Default=case wf:q(default_language) of
      "on"->true;
      _->false
    end,
    add_language(wf:q(id),IconName,Default),
    coldstrap:close_modal(),
    wf:wire(#event{postback={?MODULE, languages, show}, delegate=?MODULE});

event({?MODULE, block, move, Old}) -> % {{{2
    db:full_delete(Old),
    common:script("","$('#tree').save_parent();"),
    event({?MODULE, block, save, Old});
event({?MODULE, block, copy, #cms_mfa{id={PID, _}}=Old}) -> % {{{2
    OldBlockName=db:extract_mfa_block_name(Old),
    NewBlockName=common:q(block, ""),
    case (OldBlockName /= NewBlockName) or (PID /= wf:q(add_page_select)) of
      true->
        common:script("","$('#tree').save_parent();"),
        event({?MODULE, block, save, Old#cms_mfa{sort=new}});
      _ ->
        wf:wire(#alert { text="You need change Block name!" })
    end;
event({?MODULE, block, maybe_copy_all_structure, B}) -> % {{{2
  OldBlockName=db:extract_mfa_block_name(B),
  admin:new_modal(
        "Please enter the values", 
        {?MODULE, block, copy_all_structure, B},
        [#panel{body=[
              #p{text="Enter a New Block name:"},
               #txtbx{id=copy_block_name, text=OldBlockName},
               #p{text="Enter a Postfix for the subelements name:"},
               #txtbx{id=copy_postfix, text="_copy"}],
            class="container2"
          }
        ]
    );
event({?MODULE, block, copy_all_structure, #cms_mfa{id={PID, _},mfa=MFA}=OldBlock}) -> % {{{2
    OldBlockName=db:extract_mfa_block_name(OldBlock),
    CopyingBlock=common:q(copy_block_name, undefined),
    Postfix=wf:q(copy_postfix),
    case ((CopyingBlock==OldBlockName) or (CopyingBlock==undefined)) and (PID==wf:q(add_page_select)) of
      true->
        wf:wire(#alert { text="You need change Block name!" });
      _ ->
        common:script("","$('#tree').save_parent();"),
        {_,F,_}=parse_mfa_field(MFA),
        copy_subtree(PID, OldBlockName, F==dropdown, CopyingBlock, Postfix),
        event({?MODULE, block, save, OldBlock#cms_mfa{sort=new}}),
        coldstrap:close_modal()
    end;
event({?MODULE, block, save, #cms_mfa{id={PID, Block}, sort=S}=OldMFA}) -> % {{{2
    NewSort=
      case S of
        new -> new;
        _ ->
          try
            list_to_integer(wf:q(sort))
          catch error:badarg ->
              new
          end
      end,
    mnesia:transaction( %% remove dublicating sort
        fun() ->
          case mnesia:select(cms_mfa, 
                                  [{#cms_mfa{id={PID, Block}, active=true, sort=$1, _='_'},
                                    [
                                      {'or', {'==', '$1', S}, {'==', '$1', NewSort}}
                                    ],
                                    []}
                                  ]
                                ) of
            L when is_list(L), length(L) > 0 ->
                [ db:full_delete(Mfa) || Mfa<-L];
            _ -> undefined
          end
        end),
    MFA=OldMFA#cms_mfa{sort=NewSort},
    Saved=db:save(maybe_fix_sort(
                    apply_element_transform(
                      rec_from_qs(MFA)),
                    MFA)),
    NewMFABlock = common:q(block, undefined),
    OldMFABlock = db:extract_mfa_block_name(OldMFA),
    
    % maybe update children
    case NewSort of 
      new -> undefined;
      _ -> db:update_children(PID, NewMFABlock, OldMFABlock)
    end,

    % update pages where block exists
    case PID of
      "*" ->
            Pages=db:get_indexed_pages(),
            lists:foreach(fun(Page) ->
                              db:save(Page)
                          end, Pages);
      PID ->
            [Page]=db:get_page(PID),
            db:save(Page)
    end,
    PID_move = common:q(add_page_select, "index"),
    % wf:wire(#event{postback={?MODULE, block, make_active, Saved, PID, MFA, true, 2}, delegate=?MODULE}),

    {SelectedBlock, _} = get_selected_block(),
    SelectedBlockName = case SelectedBlock of
      Text when is_list(Text) ->
        Text;
      #cms_mfa{id={_PID, BID}} ->
          BID
    end,
    AddToBlock = common:q(add_block, "body"),
    % ?LOG("~nSelectedBlockName:~p; AddToBlock:~p",[SelectedBlockName, AddToBlock]),
    case (SelectedBlockName == AddToBlock) and (PID_move == PID) of
      false ->
        common:script("","$('#tree').html('<div>&nbsp;loading...</div>');"),
        wf:wire(#event{postback={?MODULE, page, construct, PID_move, [Block]}, delegate=?MODULE});
      true ->
        common:script("","$('#tree').remove_tree();")
    end,
    % coldstrap:close_modal(),
    % 
    case wf:qs(page) of
      ["admin"] -> "";
      _ -> update_block_on_page(Saved)
    end;
    
event({?MODULE, block, remove, B}) -> % {{{2
    admin:new_modal(
        "Are you sure to delete?", 
        {block, remove_block, B},
        []
    );
event({?MODULE, block, remove_block, B}) -> % {{{2
    {_PID, Block} = B#cms_mfa.id,
    db:maybe_delete(B),
    common:script("","$('#tree').save_parent();"),
    wf:session(selected_block, {Block, Block}),
    common:script("","$('#tree').remove_active();"),
    wf:update(edited_block, #panel{class="collapse"})
    % wf:wire(#event{postback={?MODULE, page, construct, PID, [Block]}, delegate=?MODULE}),
    % coldstrap:close_modal()
    ;
event({?MODULE, user, show}) -> % {{{2
    wf:session(history, undefined),
    CRUD = #crud{
              pagination_class=["btn", "btn-default"],
              button_class=["btn", "btn-link"],
              table_class=["table-striped", "table-bordered", "table-hover"],
              start=0,
              count=10,
              cols=[
                    {email, "Email", tb},
                    {role, "User role", {select, cms_roles()}}
                   ],
              funs=#{
                list => fun db:get_users/0,
                update => fun db:update_map/1, 
                delete => fun db:delete/1
               }
             },
    wf:update(container, [
                          #bs_row{
                             body=[#bs_col{
                                      cols=[{lg, 2},{md, 3}, {sm, 3}],
                                      body=#button{
                                              text="Create User",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({?MODULE, user, new}, ?MODULE)
                                             }}
                                  ]},
                          #bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]),
    wf:update(title, #panel{text="Users"});
event({?MODULE, user, new}) -> % {{{2
    Page = wf:state(page),
    new_modal("Create User", 
              {?MODULE, user, save},
              undefined,
              [
               account:email_field(Page),
               account:password_field(Page),
               account:retype_password_field(Page),
               #span{text="Role"},
               #dd{
                  id=role,
                  value=undefined,
                  options=cms_roles()
                 }
              ]);
event({?MODULE, user, save}) -> % {{{2
    account:event({auth, register}),
    coldstrap:close_modal(),
    wf:wire(#event{postback={?MODULE, user, show}, delegate=?MODULE});
event({?MODULE, role, show}) -> % {{{2
    wf:session(history, undefined),
    CRUD = #crud{
              pagination_class=["btn", "btn-default"],
              button_class=["btn", "btn-link"],
              table_class=["table-striped", "table-bordered", "table-hover"],
              start=0,
              count=10,
              cols=[
                    {name, "Role name", tb},
                    {sort, "Role priority", tb}
                   ],
              funs=#{
                list => fun db:get_roles/0,
                update => fun db:update_map/1, 
                delete => fun db:delete/1
               }
             },
    wf:update(container, [
                          #bs_row{
                             body=[#bs_col{
                                      cols=[{lg, 2},{md, 3}, {sm, 3}],
                                      body=#button{
                                              text="Create Role",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({?MODULE, role, new}, ?MODULE)
                                             }}
                                  ]},
                          #bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]),
    wf:update(title, #panel{text="Roles"});
event({?MODULE, role, new}) -> % {{{2
    new_modal("Create Role", 
              {?MODULE, role, save},
              undefined,
              [
               #span{text="Role"},
               #txtbx{id=name,
                      placeholder="Role name"},

               #span{text="Priority"},
               #txtbx{id=priority,
                      placeholder="Access priority for role (int)"}
              ]);
event({?MODULE, role, save}) -> % {{{2
    Name = wf:q(name),
    Role = wf:to_atom(string:to_lower(re:replace(Name, "\s", "_", [{return, list}]))),
    Priority = wf:to_integer(wf:q(priority)),
    db:save(#cms_role{
               role = Role,
               name = Name,
               sort = Priority}),
    coldstrap:close_modal(),
    wf:wire(#event{postback={?MODULE, role, show}, delegate=?MODULE});
event({?MODULE, pages, export}) -> % {{{2
    Path = "/tmp/" ++ wf:temp_id(),
    ok=mnesia:backup(Path),
    wf:redirect("/backup?path=" ++ Path);
event({?MODULE, pages, import}) -> % {{{2
    new_modal("Upload Backup",
              {popup, close},
              backup,
              undefined);
event({?MODULE, pages, merge}) -> % {{{2
    new_modal("Upload Backup",
              {popup, close},
              merge_backup,
              undefined);
event({?MODULE, db, defragment}) -> % {{{2
    db:remove_blocks_without_parent();
event({?MODULE, db, clean}) -> % {{{2
    db:remove_old_unused_blocks();
event({?MODULE, auth, call_restore_password}) -> % {{{2
    account:call_restore_password();
event({?MODULE, page, close_and_construct, PID, [Block]}) -> % {{{2
    coldstrap:close_modal(),
    event({?MODULE, page, construct, PID, [Block]});
event({?MODULE, block, make_active, Block, PID, MFA, SearchSublink, Lvl}) -> % {{{2
    wf:session(selected_block, {Block, MFA}),
    % ?LOG("make_active:~p; MFA:~p", [Block, MFA]),
    case Block of
      Text when is_list(Text) ->
          wf:update(edited_block,
            #panel{class="collapse"}
          )
          , SubTree=build_list(PID, Block, 2, false, 3),
          wf:wire(#insert_after{target=updating_subtree_lvl1, elements=SubTree}),
          common:script("","$('#tree').update_tree();")
          ;
      _ -> %#cms_mfa{id={_PID,_BID}} 
          event({?MODULE, block, edit, Block}),
          % BlockName = db:extract_mfa_block_name(Block), TODOn replaced to MFA!
          SubTree=build_list(PID, MFA, Lvl, SearchSublink, Lvl+1),
          wf:wire(#insert_after{target=updating_subtree, elements=SubTree}),
          common:script("","$('#tree').update_tree();")
    end;
    
event(close_modal) -> % {{{2
    coldstrap:close_modal();
event(Ev) -> % {{{2#
    ?LOG("Unhandled ~p event ~p", [?MODULE, Ev]),
    "".

inplace_textbox_event({asset, Record, Field}, Value) -> % {{{2
    Val = db:update(Record, Field, Value),
    Val;
inplace_textbox_event(_Tag, Value) -> % {{{2
    Value.
start_upload_event(_Tag) -> % {{{2
    ok.
finish_upload_event(backup, _Fname, Path, _Node) -> % {{{2
    ?LOG("Import backup: ~p~n ", [_Fname]),
    {atomic, _}=mnesia:restore(Path, [{clear_tables, [cms_mfa, cms_template, cms_asset, cms_page]}, {default_op, skip_tables}]),
    coldstrap:close_modal(),
    file:delete(Path);
finish_upload_event(merge_backup, _Fname, Path, _Node) -> % {{{2
    ?LOG("Merge backup: ~p~n ", [_Fname]),
    db:merge_backup_and_db(Path, mnesia_backup),
    coldstrap:close_modal(),
    file:delete(Path);
finish_upload_event(template, Fname, Path, _Node) -> % {{{2
    NewPath = wf:f("templates/~s", [Fname]),
    file:rename(Path, NewPath),
    maybe_set(name, Fname),
    wf:set(path, NewPath);

finish_upload_event(asset, Fname, Path, _Node) -> % {{{2
    #cms_asset{type=Type} = file_to_asset(Fname, ""),
    [Ext, _Min | _Id] = lists:reverse(string:tokens(Fname, ".")),
    URLPath = case string:to_lower(Ext) of
      "js" -> wf:f("js/~s", [ Fname]);
      _ -> wf:f("~s/~s", [Type, Fname])
    end,
    NewPath = wf:f("static/~s", [URLPath]),
    file:rename(Path, NewPath),
    maybe_set(name, Fname),
    wf:set(type, Type),
    wf:set(path, URLPath).

api_event(Name, Tag, Args) -> % {{{2
    ?LOG("~p API event ~p(~p; ~p)", [?MODULE, Name, Tag, Args]),
    "".

sort_event({PID, Block}, Blocks) -> % {{{2
    lists:foreach(fun({N, {block, _PID, B}}) ->
                          db:update(B, B#cms_mfa{sort=N})
                  end,
                  lists:zip(lists:seq(1, length(Blocks)), Blocks)),
    wf:wire(#event{postback={?MODULE, page, construct, PID, [Block]}, delegate=?MODULE});
sort_event(SortTag, Blocks) -> % {{{2
    wf:warning("Wrong sort event in ~p tag: ~p Blocks: ~p", [?MODULE, SortTag, Blocks]).

%% Dropdown formatters {{{1
modules() -> % {{{2
    Modules = common:module_by_function({main, 0}),
    lists:map(fun(M) -> {M, M:description()} end, Modules).

cms_roles() -> % {{{2
    [{Id, Name} || #{ role := Id, name := Name} <- db:get_roles()].

collections() -> % {{{2
    Modules = common:module_by_function({functions, 0}),
    lists:map(fun(M) -> {M, M:description()} end, Modules).

update_block_on_page(#cms_mfa{id={_,Block},mfa=MFA,sort=S}) -> % {{{2
    NewText=case MFA of 
      {common, text, Text} -> Text;
      _-> undefined
    end,
    ChangedBlock=common:block_to_html_id(wf:f("~s-~p", [Block, S])),
    wf:wire(#update{target=ChangedBlock, elements=NewText}).

admin_logout_button()-> % {{{2
  #btn{
        body="<i class='fa fa-sign-out fa-fw'></i>",
        class=["btn-logout"],
        title="Logout",
        postback={auth, logout},
        delegate=account
  }.

update_session_history(Block) -> % {{{2
%% @doc "maybe update session history"
  [LastHistory,Tail]=
    case wf:session(history) of 
      undefined -> [[],undefined];
      BL when length(BL)>7 ->
        [lists:sublist(BL,2,7), lists:last(BL)];
      SL -> [SL, lists:last(SL)]
    end,
  if Tail/=Block ->
    wf:session(history, lists:append(LastHistory, [Block]));
  true -> ok
  end,
  LastHistory.


build_html_tree_from_mfa(PID) -> % {{{2
%% @doc "Function for building main html list from tree of cms_mfa table and calls function to deep building"
  BlockWithoutParents=lists:sort(db:get_blocks_without_parent(PID)),
  MbFilterBlocks=case PID of
    "*" -> BlockWithoutParents;
    _ -> lists:delete("router", BlockWithoutParents)
  end,
  MaxLvl=3,
  % ?LOG("MbFilterBlocks ~p",[MbFilterBlocks]),
  lists:map(
    fun(BlockId) ->
        #list{
            body=#listitem{
               body=[
                      % common:icon("glyphicon", "indicator", ["glyphicon-plus-sign"]),
                      #link{text=BlockId, actions=?POSTBACK({?MODULE, block, make_active, BlockId, PID, [], false, 2}, ?MODULE)},
                      build_list(PID, BlockId, 2, false, MaxLvl)
               ],
               class=["branch lvl-1"]
            },
            class=["tree treed"]}
    end, MbFilterBlocks
  ).

build_list(PID, Block, Lvl, SearchSublink, MaxLvl) -> % {{{2
%% @doc "Recursive function for building html list from tree of cms_mfa table"
  % MaxLvl=99,
  Functions_with_sub_blocks=[dropdown,email_field,password_field,retype_password_field],
  case {db:get_mfa(PID, Block, true, SearchSublink), (Lvl<MaxLvl)} of 
    {[], _} ->  undefined;
    {_, false} -> 
      undefined;
    {L, true} ->
      SortedList=lists:keysort(#cms_mfa.sort, L),
      lists:map(
        fun(#cms_mfa{mfa=MFA,sort=S}=B)->
          {M,F,Args}=parse_mfa_field(MFA),
          ChildBlocks=case check_if_block_can_has_subblocks(B) of
            true  ->
              case F of
                panel ->
                  [Ph,Pb,Pa,Pf|_]=Args,
                  [{I,B} || I <- [Ph,Pb,Pa,Pf], I /= []];
                modal ->
                  [PB,Mt,Mb,Mf|_]=Args,
                  [{I,B} || I <- [PB,Mt,Mb,Mf], I /= []];
                article ->
                  [Ah,Ab,Af|_]=Args,
                  [{I,B} || I <- [Ah,Ab,Af], I /= []];
                _ ->[H|_]=Args,
                    [{H,B}]
              end;
            _ -> case F of 
                  Asset when (Asset==img) or (Asset==asset) -> 
                      [{wf:f("~s", [string:join(lists:reverse(lists:nth(1,Args)), ".")]), B}];
                  template ->
                      [{wf:f("~s",[Args]),B}];
                  text ->
                      [{wf:f("~s-~s-~p",[M,F,S]),B}];
                  empty ->
                      [{"empty_mfa",B}];
                  _ ->
                      [{wf:f("~s-~p",[F,S]),B}]
                 end
          end,
          ViewFunction=#span{text=wf:f(" (~s:~s)",[M,F]),
                             class="sub-label"},
          WithSublink=lists:member(F,Functions_with_sub_blocks),
          Items=lists:map(
            fun({ChildBlock,PBlock})->
              #listitem{
                     body=[
                           #link{body=[
                                      ViewFunction
                                      ],
                                text=case ChildBlock of 
                                  [] -> wf:f("~s",[F]);
                                  _ -> ChildBlock
                                end,
                                class=["not-tree"],
                                actions=?POSTBACK({?MODULE, block, make_active, PBlock, PID, ChildBlock, WithSublink, Lvl}, ?MODULE)
                           },
                           build_list(PID, ChildBlock, Lvl+1, WithSublink, MaxLvl)
                     ],
                     class=["branch lvl-"++wf:f("~p",[Lvl])]
              }
            end, ChildBlocks
          ),
          #list{body=Items}
        end, SortedList
      )
  end.

copy_subtree(PID, Block, SearchSublink, NewBlockName, Postfix) -> % {{{2
%% @doc "Recursive function for the coping a block structure"
  Functions_with_sub_blocks=[dropdown,email_field,password_field,retype_password_field],
  NewPID = common:q(add_page_select, PID),
  case db:get_mfa(PID, Block, true, SearchSublink) of 
    [] -> undefined;
    L  ->
      SortedList=lists:keysort(#cms_mfa.sort, L),
      lists:map(
        fun(#cms_mfa{id={_,BID}}=B)->
          ChildBlocks=extract_mfa_names(B),
          
          lists:map(
            fun({ChildBlock, #cms_mfa{mfa=MFA}=PBlock})->
              {M,F,Args}=parse_mfa_field(MFA),
              NewBID=case NewBlockName of
                undefined-> BID++Postfix;
                _ -> NewBlockName
              end,
              NewMfa=case check_if_block_can_has_subblocks(PBlock) of
                true  ->
                  case F of
                    panel ->
                      [A1,A2,A3,A4|A5_N]=Args,
                      NewAgs=lists:append([A1++Postfix,A2++Postfix,A3++Postfix,A4++Postfix], A5_N),
                      {M,F,NewAgs};
                    modal ->
                      [A1,A2,A3|A4_N]=Args,
                      NewAgs=lists:append([A1++Postfix,A2++Postfix,A3++Postfix], A4_N),
                      {M,F,NewAgs};
                    article ->
                      [A1,A2|A3_N]=Args,
                      NewAgs=lists:append([A1++Postfix,A2++Postfix],A3_N),
                      {M,F,NewAgs};
                    _ ->
                      [A1|A2_N]=Args,
                      NewAgs=lists:append([A1++Postfix], A2_N),
                      {M,F,NewAgs}
                    end;
                _ -> 
                  {M,F,Args}
              end,
              WithSublink=lists:member(F,Functions_with_sub_blocks),
              UpdMfa=PBlock#cms_mfa{mfa=NewMfa},
              CopyBlock = db:update_record_field(UpdMfa, id, {NewPID, NewBID}),
              db:save(CopyBlock),
              copy_subtree(PID, ChildBlock, WithSublink, undefined, Postfix)
            end, ChildBlocks
          )
        end, SortedList
      )
  end.

check_if_block_can_has_subblocks(#cms_mfa{mfa=MFA}) -> % {{{2
  {M,_,Args}=parse_mfa_field(MFA),
  Exclude_modules=[analytics,common,router],
  (length(Args) > 0) and (not lists:member(wf:to_atom(M), Exclude_modules));
check_if_block_can_has_subblocks(_Tree_head)->
  true.

parse_mfa_field(MFA) -> % {{{2
  case MFA of
    {A1,A2,A3} -> {A1,A2,A3};
    _ -> {common,empty,[]}
  end.

extract_mfa_names(#cms_mfa{mfa=MFA}=B) -> % {{{2
%% @doc "Return list [{mfa_name_in_block, cms_mfa_record}]
  {_M,F,Args}=parse_mfa_field(MFA),
  case check_if_block_can_has_subblocks(B) of
    true  ->
      case F of
        panel ->
          [Ph,Pb,Pa,Pf|_]=Args,
          [{I,B} || I <- [Ph,Pb,Pa,Pf], I /= []];
        modal ->
          [PB,Mt,Mb,Mf|_]=Args,
          [{I,B} || I <- [PB,Mt,Mb,Mf], I /= []];
        article ->
          [Ah,Ab,Af|_]=Args,
          [{I,B} || I <- [Ah,Ab,Af], I /= []];
        _ ->[H|_]=Args,
            [{H,B}]
      end;
    _ -> 
          [{undefined,B}]
  end.

get_selected_block() -> % {{{2
  SelectedBlockSess = wf:session(selected_block),
  case SelectedBlockSess of
    undefined -> {"body", "body"}; % default block for adding subblocks
    {A, []} when is_list(A)-> {A, A};
    {A1, A2} -> {A1, A2}
  end.
