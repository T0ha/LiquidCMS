%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (admin).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> 
    index:main().

title() -> "Liquid CMS - Admin".

event(click) -> % {{{1
    wf:insert_top(placeholder, "<p>You clicked the button!").

default_data() -> % {{{1
    #{cms_mfa => [
                    %Scripts
                    #cms_mfa{id={"*", "script"},
                             mfa={common, script, ["script"]},
                             sort=1},

                    %CSS
                    #cms_mfa{id={"*", "css"},
                             mfa={common, css, ["css"]},
                             sort=1}
                      ]}.

install() -> % {{{1
    {ok, StaticFolders} = application:get_env(simple_bridge, static_paths),
    %io:format("StaticFolders: ~p~n", [StaticFolders]),
    {ok, OldCWD} = file:get_cwd(),
    file:set_cwd("static"),
    Static = [{wf:f("static/~s", [Path]), Files} ||
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
    file:set_cwd(OldCWD),
    db:save(Assets),
    {ok, TemplateFiles} = file:list_dir("templates"),
    lists:foreach(fun(F) -> add_template("templates/" ++ F, []) end, TemplateFiles),
    add_page("index", "templates/index.html"),
    add_page("admin", "templates/blank.html", admin, admin).

file_to_asset(File, Path) -> % {{{1
        [Ext, Min | Id] = lists:reverse(string:tokens(File, ".")),
        case {Ext, Min} of
            {"js", "min"} ->
                #cms_asset{
                   id=[Ext | Id],
                   file="static/js/" ++ File,
                   minified=true,
                   type=script};
            {"js", _} ->
                #cms_asset{
                   id=[Ext, Min | Id],
                   file="static/js/" ++ File,
                   type=script};
            {"css", "min"} ->
                #cms_asset{
                   id=[Ext | Id],
                   file="static/css/" ++ File,
                   minified=true,
                   type=css};
            {"css", _} ->
                #cms_asset{
                   id=[Ext, Min | Id],
                   file="static/css/" ++ File,
                   type=css};
            {Any, _} ->
                #cms_asset{
                   id=[Ext, Min | Id],
                   file="static/css/" ++ File,
                   type=binary}
        end.

    

menu_item_funs(Page, MenuBlock, ItemSub, URL) -> % {{{1
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

link_body_funs(Page, MenuBlock, ItemSub, Icon, Text) -> % {{{1
    ItemBlock = common:sub_block(MenuBlock, ItemSub),
    ItemBodyBlock = common:sub_block(ItemBlock, "body"),

    [
     case Icon of
         {_Font, _Name, _Classes} = Args ->
             A = tuple_to_list(Args),
             #cms_mfa{id={Page, ItemBodyBlock},
                      mfa={common, icon, A},
                      sort=1};
         _ ->
             []
     end,
     if Text /= undefined ->
            #cms_mfa{id={Page, ItemBodyBlock},
                     mfa={wf, f, [Text, []]},
                     sort=2};
        true -> []
     end
    ].


add_page(PID, TemplatePath) -> % {{{1
   add_page(PID, TemplatePath, undefined, index).

add_page(PID, TemplatePath, Role, Module) -> % {{{1
    Funs = [
            %fun index:maybe_redirect_to_login/1,
            fun index:maybe_change_module/1,
            fun(Page) -> common:template(Page, "page") end
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

add_template(TemplatePath, Bindings) -> % {{{1
    IsTemplate = filelib:is_regular(TemplatePath),
    if IsTemplate ->
           db:save(#cms_template{
                      file = TemplatePath,
                      bindings = Bindings});
       true ->
           {error, no_template}
    end.

%script_add(PID, Script, Sort) -> % {{{1
%    [
%    #cms_asset{
%             id = {PID, "page"},
%             %mfa = fun() -> common:script(Script) end,
%             sort = Sort}
%    ],
%    db:save(MFA).

