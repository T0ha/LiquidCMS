%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (admin).
-compile([export_all, {parse_transform, lager_transform}]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

%% CMS Module interface {{{1
description() -> % {{{2
    "Admin".

%% Module render functions {{{1
main() -> % {{{2
    index:main().

title() -> "Liquid CMS - Admin".% {{{2

body(Page) ->% {{{2
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
                  #cms_mfa{id={"*", "script"},
                           mfa={common, asset, [["js", "hotkeys", "jquery"]]},
                           sort=6,
                           settings=#{filters => ["", "", "editor"]}},
                  #cms_mfa{id={"*", "script"},
                           mfa={common, asset, [["js", "bootstrap-wysiwyg"]]},
                           sort=7,
                           settings=#{filters => ["", "", "editor"]}},
                  #cms_mfa{id={"*", "script"},
                           mfa={common, asset, [["js", "jquery.hotkeys"]]},
                           sort=8,
                           settings=#{filters => ["", "", "admin"]}},
                  #cms_mfa{id={"*", "script"},
                           mfa={common, asset, [["js", "bootstrap-wysiwyg"]]},
                           sort=9,
                           settings=#{filters => ["", "", "admin"]}},

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
    get_files_from_folder("static"),
    get_files_from_folder("templates"),

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

    add_navbar_button("admin", "sidebar-nav", "accounts", {{"fa", "users", []}, "Access"}, {menu, "accounts-menu"}),
    add_navbar_button("admin", "accounts-menu", "users-all", {{"fa", "user", []}, "Users"}, {event, ?POSTBACK({user, show})}),
    add_navbar_button("admin", "accounts-menu", "roles-all", {{"fa", "group", []}, "Roles"}, {event, ?POSTBACK({role, show})}),

    ok.

%% Different components adding to pages  {{{1
add_page(PID, TemplatePath) -> % {{{2
    add_page(PID, TemplatePath, undefined, index).

add_page(PID, TemplatePath, Role, Module) -> % {{{2
    lager:info("Installing ~p module", [?MODULE]),
    add_page(PID, <<"LiquidCMS">>, "LiquidCMS", Role, Module),
    add_to_block(PID, "body", {template, TemplatePath}).

add_page(PID, Title, Description, Role, Module) -> % {{{2

    %Funs = [
    %        {index, maybe_redirect_to_login, []},
    %        {index, maybe_change_module, []},
    %        {common, template, ["templates/main.html"]}
    %       ],
    %NFuns = lists:zip(lists:seq(1, length(Funs)), Funs),
    %MFAs = [#cms_mfa{
    %           id = {PID, "page"},
    %           mfa=F,
    %           sort=N} || {N, F} <- NFuns],

    Page = #cms_page{
              id=PID,
              title=Title,
              description=Description,
              accepted_role=Role,
              module=Module
             },
    ?LOG("~nadd_page:~p", [Page]),
    db:save(Page).

add_template(TemplatePath, Bindings) -> % {{{2
    add_template(TemplatePath, TemplatePath, TemplatePath, Bindings).

add_template(Name, Description, TemplatePath, Bindings) -> % {{{2
    IsTemplate = filelib:is_regular(TemplatePath),
    CT = calendar:universal_time(),
    if IsTemplate ->
           db:save(#cms_template{
                      file = TemplatePath,
                      name = Name,
                      description = Description,
                      bindings = Bindings,
                      created_at=CT,
                      updated_at={}
                     });
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
    _ItemSubmenuBlock = common:sub_block(ItemBlock, "submenu"),
    ButtonMFAs = [
                  #cms_mfa{id={PID, MenuBlock},
                           mfa={common,
                                list_item,
                                [ItemBlock]},
                           sort=1},
                  #cms_mfa{id={PID, "body"},
                           mfa={common, script, [wf:f("$('#~s').metisMenu();", [MenuBlock])]},
                           sort=1},
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
    db:save(
      db:fix_sort(ButtonMFAs ++ LinkMFAs));
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
    db:save(
      db:fix_sort(ButtonMFAs ++ LinkMFAs));
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
    db:save(
      db:fix_sort(ButtonMFAs ++ LinkMFAs)).

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
            ?LOG("~s vaue is ~p", [Id, A])
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
       postback=close_modal
      }.

render_copy_button(undefined) -> % {{{2
    "";
render_copy_button(CopyPostback) -> % {{{2
    #btn{
       type=success,
       size=md,
       text="Copy",
       postback=CopyPostback
      }.

render_move_button(MovePostback) -> % {{{2
    #btn{
       id=save_button,
       type=success,
       size=md,
       text="Move",
       postback=MovePostback
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
                         body=common:icon("fa", "plus", []),
                         show_if=(Sub /= undefined),
                         actions=?POSTBACK({block, add, Sub})
                        },
                      #link{
                         class="btn btn-link",
                         body=common:icon("fa", "arrow-right", []),
                         show_if=(Sub /= undefined),
                         actions=?POSTBACK({page, construct, PID, [Sub]})
                        },
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
    add_default_fields(Data, Formatting, "", "");
add_default_fields({Data, Formatting, Block, Classes}) -> % {{{2
    add_default_fields(Data, Formatting, Block, Classes);
add_default_fields(Any) when is_list(Any) -> % {{{2
    [{"", Any}, {"", []}];
add_default_fields(Any) -> % {{{2
    [{"", Any}, {"", []}].

add_default_fields(Data, Formatting, Block, Classes) -> % {{{2
    [ 
     {"Data", [
               {"Block name", {block, Block}}
               | Data]},
     {"Formatting",
      Formatting ++ [{"Additional classes", {classes, Classes}}]}
    ].

form_fields(M, F, A) -> % {{{2
    try 
        apply(M, form_data, [F, A])
    catch error:E when E /= undef; 
                       E /= function_clause -> 
              [_, Block, Classes] = maybe_empty(A, 3),
              {[], [], Block, Classes}
    end.

form_elements(M, F, A) -> % {{{2
    render_fields(
      add_default_fields(
        form_fields(M, F, A))).

render_fields(Cols) -> % {{{2
    try 12 div length(Cols) of
        Width when Width >= 4 ->
            %?LOG("Width: ~p", [Width]),
            [
             #bs_row{
                body=[render_field(Col, Width) || Col <- Cols]
               }];
        _Width ->
            %?LOG("Width: ~p", [Width]),
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
                text=Text
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

get_classes(M, Prefix) -> % {{{2
    Fields = formatting_fields(form_fields(M, Prefix, [])),
    ?LOG("Classes fields: ~p", [Fields]),
    AllClasses = wf:mq([classes | Fields]),
    ?LOG("Classes data: ~p", [AllClasses]),
    %Classes = sets:to_list(sets:from_list(AllClasses)),
    lists:map(fun(none) -> "";
                 (undefined) -> "";
                 ("") -> "";
                 (Class) -> Class
              end,
              AllClasses).

get_data(M, F) -> % {{{2
    Fields = data_fields(form_fields(M, F, [])),
    ?LOG("Fields: ~p", [Fields]),
    Data = wf:mq([block | Fields]),
    ?LOG("Data: ~p", [Data]),
    lists:map(fun(none) -> "";
                 (undefined) -> "";
                 ("") -> "";
                 (Field) -> Field
              end,
              Data).

data_fields({Data, _, _, _}) -> % {{{2
    data_fields(Data);
data_fields({Data, _}) -> % {{{2
    data_fields(Data);
data_fields(Data) when is_list(Data) -> % {{{2
    lists:flatten([get_fields(F) || F <- Data, get_fields(F) /= []]).

formatting_fields({_, Formats, _, _}) -> % {{{2
    [get_fields(F) || F <- Formats, get_fields(F) /= []];
formatting_fields({_, Formats}) -> % {{{2
    [get_fields(F) || F <- Formats, get_fields(F) /= []];
formatting_fields(Any) -> % {{{2
    ?LOG("Other fields: ~p", [Any]),
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
get_fields(D) -> % {{{2
    ?LOG("Data: ~p", [D]),
    [].

prefix_classes(Prefix, Classes) -> % {{{2
    [wf:f("~s-~s", [Prefix, C]) || C <- Classes].

remove_prefix([]) -> % {{{2
    "";
remove_prefix(Class) -> % {{{2
    lists:last(string:split(Class, "-")).

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
    %io:format("Folders: ~p~n", [Folders]),
    {ok, OldCWD} = file:get_cwd(),
    file:set_cwd(SubFolder),
    Static = [{wf:f("~s", [Path]), Files} ||
              Path <- StaticFolders,
              filelib:is_dir(Path),
              {ok, Files} <- [file:list_dir(Path)],
              Path /= "nitrogen"],
    %io:format("Static: ~p~n", [Static]),
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
    file:set_cwd(OldCWD);

get_files_from_folder(SubFolder) -> % {{{2
    {ok, TemplateFiles} = file:list_dir(SubFolder),
    lists:foreach(fun(F) -> add_template(wf:f("~s/~s", [SubFolder, F]), []) end, TemplateFiles).

get_filters(#cms_mfa{settings=#{filters := Filters}}) -> % {{{2
    Filters;
get_filters(_) -> % {{{2
    ["", "", ""].

maybe_fix_sort(#cms_mfa{sort=new}=R, _) -> % {{{2
    db:fix_sort(R);
maybe_fix_sort(#cms_mfa{id=Id}=R, #cms_mfa{id=Id}) -> % {{{2
    R;
maybe_fix_sort(R, _) -> % {{{2
    db:fix_sort(R).

rec_from_qs(R) -> % {{{2
    M = wf:to_atom(common:q(module, "common")),
    F = wf:to_atom(common:q(function, "template")),
    Args = admin:get_data(M, F),

    PID = common:q(add_page_select, "index"),
    Block = common:q(add_block, "body"),

    Classes = admin:get_classes(M, wf:to_atom(F)),

    Filters = wf:mq([qs_key, qs_val, role]),

    R#cms_mfa{id={PID, Block}, 
              mfa={M, F, Args ++ [Classes]},
              settings=#{filters => Filters}}.

apply_element_transform(#cms_mfa{mfa={M, _, _}}=Rec) -> % {{{2
    try apply(M, save_block, [Rec])
    catch 
        error:undef -> Rec;
        error:function_clause -> Rec 
    end.
get_pages() -> % {{{2
    [#{id => "*"} | db:get_pages()].

%% Event handlers {{{1
event({common, edit, text, #cms_mfa{id={PID, Block}}=MFA, Text}) -> % {{{2
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
                        html=Text,
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
                                         ]},
                                 #panel{
                                    class="btn-group",
                                    body=[
                                          #link{
                                             class=[
                                                    "btn",
                                                    "btn-default",
                                                    "dropdown-toggle"
                                                   ],
                                             text="H<i class='caret'></i>",
                                             data_fields=[
                                                          {toggle, dropdown}
                                                         ]},
                                          #list{
                                             class="dropdown-menu",
                                             numbered=flase,
                                             body=[ 
                                                   #listitem{
                                                      body=
                                                      #wysiwyg_button{
                                                         body="H1",
                                                         func="heading h1",
                                                         class=["btn", "btn-default"]}
                                                     },
                                                   #listitem{
                                                      body=
                                                      #wysiwyg_button{
                                                         body="H2",
                                                         func="heading 2",
                                                         class=["btn", "btn-default"]}
                                                     },
                                                   #listitem{
                                                      body=
                                                      #wysiwyg_button{
                                                         body="H3",
                                                         func="heading 3",
                                                         class=["btn", "btn-default"]}
                                                     },
                                                   #listitem{
                                                      body=
                                                      #wysiwyg_button{
                                                         body="H4",
                                                         func="heading 4",
                                                         class=["btn", "btn-default"]}}
                                                  ]}
                                         ]}
                                ]}
              ]);

event({asset, new, _Type}) -> % {{{2 TODO: Check and remove _Type at all
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
event({asset, refresh, Type}) -> % {{{2
    get_files_from_folder("static"),
    event({asset, show, Type});
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
                                      cols={lg, 8},
                                      body=#h1{text=wf:f("Static Assets: ~s", [Type])}
                                     },
                                   #bs_col{
                                      cols={lg, 2},
                                      body=#button{
                                              text="Refresh filesystem",
                                              class=["btn",
                                                     "btn-warning",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({asset, refresh, Type})
                                             }},
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

event({template, refresh}) -> % {{{2
    get_files_from_folder("templates"),
    event({template, show});
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
                                      cols={lg, 8},
                                      body=#h1{text="Templates"}
                                     },
                                   #bs_col{
                                      cols={lg, 2},
                                      body=#button{
                                              text="Refresh filesystem",
                                              class=["btn",
                                                     "btn-warning",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({template, refresh})
                                             }},
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
                    {module, "Module", {select, modules()}},
                    {accepted_role, "Assess role", {select, cms_roles()}},
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
                  options=modules()
                 },

               #span{text="Who Have Access"},
               #dd{
                  id=role,
                  value=undefined,
                  options=cms_roles()
                 }
              ]);

event({page, construct}) -> % {{{2
    Pages = get_pages(),
    [#{id := P} | _] = Pages,
    PID = common:q(page_select, P),
    ?LOG("~nconstruct page:~p",[PID]),
    Block = common:q(block_select, "page"),
    wf:wire(#event{postback={page, construct, PID, [Block]}});

event({page, construct, PID, [Block|_]}) -> % {{{2
    Pages = get_pages(),
    Blocks = [format_block(B#cms_mfa{id={PID, BID}})
              || #cms_mfa{id={_, BID}}=B <- db:get_mfa(PID, Block)],
    AllBlocks = db:get_all_blocks(PID),
    ?LOG("~nconstruct page(2):~p",[PID]),

    ShowAll = (common:q(show_all, "false") /= "false"),

    PageSelect = [
                  #dropdown{
                     id=page_select,
                     options=lists:keysort(1,  [{N, N} || #{id := N} <- Pages]),
                     value=PID,
                     postback={page, construct}
                    },
                  #span{text=" / "},
                  #dropdown{
                     id=block_select,
                     options=lists:keysort(1, [{N, N} ||  N <- AllBlocks, not common:is_private_block(N) or ShowAll, (N /= "router") or (PID == "*")]),
                     value=Block,
                     postback={page, construct}
                    },
                  #span{text=" Show All "},
                  #checkbox{
                     text="",
                     id=show_all,
                     checked=ShowAll,
                     postback={page, construct}
                    },
                  #btn{
                     text="Export Pages",
                     class=["pull-right"],
                     style="",
                     type=success,
                     actions=?POSTBACK({?MODULE, pages, export})
                    },
                  #btn{
                   text="Merge Pages",
                   class=["pull-right"],
                   style="margin: 0 10px;",
                   type=info,
                   actions=?POSTBACK({?MODULE, pages, merge})
                  },
                  #btn{
                     text="Import Pages",
                     class=["pull-right"],
                     type=warning,
                     actions=?POSTBACK({?MODULE, pages, import})
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

event({page, save}) -> % {{{2 onclick <Save> btn
    PID = wf:q(name),
    ?LOG("~nevent savve page:~p",[PID]),
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
                  options=lists:keysort(2, apply(M, functions, []))
                 }),
    wf:wire(#event{postback={block, change, function}});
event({block, change, function}) -> % {{{2
    M = wf:to_atom(common:q(module, common)),
    F = wf:to_atom(common:q(function, common)),
    ?LOG("M: ~p, F: ~p", [M, F]),
    wf:update(block_data, admin:form_elements(M, F, []));
event({block, add}) -> % {{{2
    PID = common:q(page_select, "index"),
    Block = common:q(block_select, "body"),
    B = #cms_mfa{
           id={PID, Block},
           mfa={common, template, ["templates/login.html"]},
           sort=new},
    event({block, edit, B});
event({block, add, Block}) -> % {{{2
    PID = common:q(page_select, "index"),
    B = #cms_mfa{
           id={PID, Block},
           mfa={common, template, ["templates/login.html"]},
           sort=new},
    event({block, edit, B});
event({block, edit, #cms_mfa{id={PID, Block}, mfa={M, F, A}}=B}) -> % {{{2
    Pages = get_pages(),
    [#{id := _P} | _] = Pages,
    [QSKey, QSVal, Role] = get_filters(B),
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
              {?MODULE, block, move, B},
              {?MODULE, block, copy, B},
              undefined, 
              [
               #span{text="Elements Collection"},
               #dd{id=module,
                   value=M,
                   postback={block, change, module},
                   options=collections()},

               #span{text="Block Type"},
               #dd{
                  id=function,
                  value=F,
                  postback={block, change, function},
                  options=(M):functions()
                 },
               #panel{
                  id=block_data,
                  body=form_elements(M, F, [PID | A])
                 },
               #panel{
                  body=render_fields([{"Filters", 
                                       [
                                        {"Query String", 
                                         [
                                          {"Key", {qs_key, QSKey}},
                                          {"Value", {qs_val, QSVal}}
                                         ]},
                                        {"Role", {role, Role}}
                                       ]}])
                 }
              ]);

event({?MODULE, block, move, Old}) -> % {{{2
    db:delete(Old),
    event({?MODULE, block, save, Old});
event({?MODULE, block, copy, Old}) -> % {{{2
    event({?MODULE, block, save, Old#cms_mfa{sort=new}});
event({?MODULE, block, save, OldMFA}) -> % {{{2
    [#cms_mfa{id={PID, Block}}|_] = common:maybe_list(
                                      db:save(
                                        maybe_fix_sort(
                                          apply_element_transform(
                                            rec_from_qs(OldMFA)),
                                          OldMFA))),

    coldstrap:close_modal(),
    wf:wire(#event{postback={page, construct, PID, [Block]}});
event({block, remove, B}) -> % {{{2
    admin:new_modal(
        "Are you sure to delete?", 
        {block, remove_block, B},
        []
    );

event({block, remove_block, B}) -> % {{{2
    {PID, Block} = B#cms_mfa.id,
    db:maybe_delete(B),
    wf:wire(#event{postback={page, construct, PID, [Block]}}),
    coldstrap:close_modal();
event({user, show}) -> % {{{2
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
                             body=[
                                   #bs_col{
                                      cols={lg, 10},
                                      body=#h1{text="Users"}
                                     },
                                   #bs_col{
                                      cols={lg, 2},
                                      body=#button{
                                              text="Create User",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({user, new})
                                             }}
                                  ]},
                          #bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]);
event({user, new}) -> % {{{2
    Page = wf:state(page),
    new_modal("Create User", 
              {user, save},
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
event({user, save}) -> % {{{2
    account:event({auth, register}),
    coldstrap:close_modal(),
    wf:wire(#event{postback={user, show}});
event({role, show}) -> % {{{2
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
                             body=[
                                   #bs_col{
                                      cols={lg, 10},
                                      body=#h1{text="Roles"}
                                     },
                                   #bs_col{
                                      cols={lg, 2},
                                      body=#button{
                                              text="Create Role",
                                              class=["btn",
                                                     "btn-success",
                                                     "btn-block",
                                                     "btn-upload"],
                                              actions=?POSTBACK({role, new})
                                             }}
                                  ]},
                          #bs_row{
                             body=#bs_col{
                                     cols={lg, 12},
                                     body=CRUD
                                    }
                            }]);
event({role, new}) -> % {{{2
    new_modal("Create Role", 
              {role, save},
              undefined,
              [
               #span{text="Role"},
               #txtbx{id=name,
                      placeholder="Role name"},

               #span{text="Priority"},
               #txtbx{id=priority,
                      placeholder="Access priority for role (int)"}
              ]);
event({role, save}) -> % {{{2
    Name = wf:q(name),
    Role = wf:to_atom(string:to_lower(re:replace(Name, "\s", "_", [{return, list}]))),
    Priority = wf:to_integer(wf:q(priority)),
    db:save(#cms_role{
               role = Role,
               name = Name,
               sort = Priority}),
    coldstrap:close_modal(),
    wf:wire(#event{postback={user, show}});
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
event({auth, call_restore_password}) -> % {{{2
    account:call_restore_password();
event(close_modal) ->
  coldstrap:close_modal();
event(Ev) -> % {{{2
    ?LOG("~p event ~p", [?MODULE, Ev]).

inplace_textbox_event({asset, Record, Field}, Value) -> % {{{2
    ?LOG("~n inplace_textbox_event ~p", [Record]),
    Val = db:update(Record, Field, Value),
    Val;
inplace_textbox_event(Tag, Value) -> % {{{2
    ?LOG("~p inplace tb event ~p: ~p", [?MODULE, Tag, Value]),
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
    URLPath = wf:f("~s/~s", [Type, Fname]),
    NewPath = wf:f("static/~s", [URLPath]),
    file:rename(Path, NewPath),
    maybe_set(name, Fname),
    wf:set(type, Type),
    wf:set(path, URLPath).

api_event(Name, Tag, Args) -> % {{{2
    ?LOG("~p API event ~p(~p; ~p)", [?MODULE, Name, Tag, Args]).

sort_event({PID, Block}, Blocks) -> % {{{2
    % ?LOG("Blocks: ~p", [Blocks]),
    lists:foreach(fun({N, {block, _PID, B}}) ->
                          db:update(B, B#cms_mfa{sort=N})
                  end,
                  lists:zip(lists:seq(1, length(Blocks)), Blocks)),
    wf:wire(#event{postback={page, construct, PID, [Block]}});
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

