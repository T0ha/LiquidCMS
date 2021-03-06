-module(db).
%%%
%%% Mnesia DB backend routines module.
%%% Put your backend code here
%%%

-compile([export_all]).
-include("db.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("cms.hrl").

%% Don't remove! This is is used to install your Mnesia DB backend  from CLI tool
install([])-> % {{{1
    ?CREATE_TABLE(cms_settings, set, []),
    ?CREATE_TABLE(cms_mfa, bag, []),
    ?CREATE_TABLE(cms_template, set, []),
    ?CREATE_TABLE(cms_asset, bag, [type, name, file]),
    ?CREATE_TABLE(cms_page, set, []),
    ?CREATE_TABLE(cms_user, set, []),
    ?CREATE_TABLE(cms_role, set, []),
    ?CREATE_TABLE(cms_form, set, []),
    ?CREATE_TABLE(cms_language, set, []),
    {ok, VSN} = application:get_key(nitrogen, vsn),
    DataModules = common:module_by_function({default_data, 0}),
    mnesia:transaction(
      fun() ->
              mnesia:write(#cms_settings{key=vsn, value=VSN}),
              [maps:map(
                 fun(_K, V) ->
                         lists:foreach(
                           fun(#cms_mfa{}=R) ->
                                   mnesia:write(
                                     fix_sort(
                                       update_timestamps(R)));
                              (R) ->
                                   mnesia:write(
                                       update_timestamps(R))
                           end,
                           lists:flatten(V))
                 end,
                (M):default_data()) ||
               M <- DataModules]
      end).
%% Don't remove! This is is used to update your Mnesia DB backend  from CLI tool
update([]) -> % {{{1
    {ok, VSN} = application:get_key(nitrogen, vsn),
    case get_db_vsn() of
        VSN -> ok;
        _ ->
            update(VSN)
    end;
update("0.1.0"=VSN) -> % {{{1
    ?CREATE_TABLE(cms_settings, set, []),
    transaction(fun() ->
                        A = mnesia:match_object(#cms_mfa{mfa={index, maybe_redirect_to_login, '_'}, _ = '_'}),
                        B = mnesia:match_object(#cms_mfa{mfa={index, maybe_change_module, '_'}, _ = '_'}),
                        [mnesia:delete_object(O) || O <- A ++ B]
                end),
    mnesia:transform_table(cms_user, fun({cms_user, E, P, R, S}) -> 
                                             #cms_user{
                                                email=E,
                                                password=P,
                                                role=R,
                                                confirm=0,
                                                settings=S
                                               }
                                     end, record_info(fields, cms_user)),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("0.1.1"=VSN) -> % {{{1
    ?CREATE_TABLE(cms_settings, set, []),
    mnesia:backup("mnesia.lcms"),
    mnesia:delete_table(cms_user),
    ?CREATE_TABLE(cms_user, set, []),
    mnesia:restore("mnesia.lcms", []),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("0.1.2"=VSN) -> % {{{1
    ?CREATE_TABLE(cms_settings, set, []),
    CT = calendar:universal_time(),
    mnesia:transform_table(cms_mfa, fun({cms_mfa, Id,Sort,M,S}) -> 
                                         #cms_mfa{
                                            id=Id,
                                            sort=Sort,
                                            mfa=M,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CT
                                           }
                                    end, record_info(fields, cms_mfa)),
    mnesia:transform_table(cms_template, fun({cms_template, F,B,N,D,S}) -> 
                                         #cms_template{
                                            file=F,
                                            bindings=B,
                                            name=N,
                                            description=D,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CT
                                           }
                                    end, record_info(fields, cms_template)),
    mnesia:transform_table(cms_asset, fun({cms_asset, Id,N,D,F,M,T,S}) -> 
                                         #cms_asset{
                                            id=Id,
                                            name=N,
                                            description=D,
                                            file=F,
                                            minified=M,
                                            type=T,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CT
                                           }
                                    end, record_info(fields, cms_asset)),
    mnesia:transform_table(cms_page, fun({cms_page, Id,D,M,Ar,T,S}) -> 
                                         #cms_page{
                                            id=Id,
                                            description=D,
                                            module=M,
                                            accepted_role=Ar,
                                            title=T,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CT
                                           }
                                    end, record_info(fields, cms_page)),
    mnesia:transform_table(cms_user, fun({cms_user, E, P, R, C, S}) -> 
                                         #cms_user{
                                            email=E,
                                            password=P,
                                            role=R,
                                            confirm=C,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CT
                                           }
                                    end, record_info(fields, cms_user)),
    mnesia:transform_table(cms_role, fun({cms_role, R,N,Sort,S}) -> 
                                         #cms_role{
                                            role=R,
                                            name=N,
                                            sort=Sort,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CT
                                           }
                                    end, record_info(fields, cms_role)),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});

update("0.1.3"=VSN) -> % {{{1
  io:format("~nUpdate to ~p~n",[VSN]),
  CurTime = calendar:universal_time(),
  mnesia:transform_table(cms_mfa, fun({cms_mfa, Id,Sort,M,CT,_UT,S}) -> 
                                         #cms_mfa{
                                            id=Id,
                                            sort=Sort,
                                            mfa=M,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CurTime,
                                            active=true
                                           }
                                    end, record_info(fields, cms_mfa)),
    mnesia:transform_table(cms_template, fun({cms_template, F,B,N,D,CT,_UT,S}) -> 
                                         #cms_template{
                                            file=F,
                                            bindings=B,
                                            name=N,
                                            description=D,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CurTime,
                                            active=true
                                           }
                                    end, record_info(fields, cms_template)),
    mnesia:transform_table(cms_asset, fun({cms_asset, Id,N,D,F,M,T,CT,_UT,S}) -> 
                                         #cms_asset{
                                            id=Id,
                                            name=N,
                                            description=D,
                                            file=F,
                                            minified=M,
                                            type=T,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CurTime,
                                            active=true
                                           }
                                    end, record_info(fields, cms_asset)),
    mnesia:transform_table(cms_page, fun({cms_page, Id,D,M,Ar,T,CT,_UT,S}) -> 
                                         #cms_page{
                                            id=Id,
                                            description=D,
                                            module=M,
                                            accepted_role=Ar,
                                            title=T,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CurTime,
                                            active=true
                                           }
                                    end, record_info(fields, cms_page)),
    mnesia:transform_table(cms_role, fun({cms_role, R,N,Sort,CT,_UT,S}) -> 
                                         #cms_role{
                                            role=R,
                                            name=N,
                                            sort=Sort,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CurTime,
                                            active=true
                                           }
                                    end, record_info(fields, cms_role)),
    mnesia:transform_table(cms_user, fun({cms_user, E, P, R, C,CT,_UT,S}) -> 
                                         #cms_user{
                                            email=E,
                                            password=P,
                                            role=R,
                                            confirm=C,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CurTime,
                                            active=true
                                           }
                                    end, record_info(fields, cms_user)),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("0.1.4"=VSN) -> % {{{1
    [AdminPage] = get_page("admin"),
    save(AdminPage#cms_page{module=index, updated_at=calendar:universal_time()}),

    transaction(fun() ->
                        NavBarEvents = mnesia:match_object(#cms_mfa{id={"admin", '_'}, mfa={html5, link_event, '_'}, _='_'}),
                        lists:foreach(fun(#cms_mfa{mfa={M, F, [Block, Event]}}=MFA) ->
                                              ok=mnesia:delete_object(MFA),
                                              mnesia:write(MFA#cms_mfa{mfa={M, F, [Block, Event#event{delegate=admin}]}})
                                      end,
                                      NavBarEvents)
                end),
    transaction(fun() ->
                        Pages = mnesia:match_object(#cms_page{accepted_role=undefined, _='_'}),
                        [mnesia:write(P#cms_page{accepted_role=nobody}) || P <- Pages]
                end),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("1.0.0"=VSN) -> 
    ?CREATE_TABLE(cms_form, set, []),
    transaction(fun() -> % {{{2 :  replace commmon -> html5
                        Replaced_items = [list, list_item, link_url, link_event, block], 
                        lists:foreach(
                          fun(Replaced) ->
                            Common_elements = mnesia:match_object(#cms_mfa{id='_', mfa={common, Replaced, '_'}, _='_'}),
                            lists:foreach(fun(#cms_mfa{mfa={_M, F, A}}=MFA) ->
                                                  New_mfa = MFA#cms_mfa{mfa={html5, F, A},
                                                                        updated_at=calendar:universal_time()},
                                                  mnesia:delete_object(MFA),
                                                  mnesia:write(New_mfa)
                                          end,
                                          Common_elements)
                          end,
                          Replaced_items)
                end),
    
    DeleteTemplates=["templates/admin.html", "templates/login.html", "templates/main.html",
                      "templates/navbar.html", "templates/setup.html", "templates/ga_analytics.html",
                      "templates/ya_analytics.html", "templates/hs_analytics.html"],
    transaction(fun() -> % delete old templates
      lists:foreach(
        fun(Tmpl) ->
          Tmpls = mnesia:match_object(#cms_template{file=Tmpl, _='_'}),
          lists:foreach(fun(#cms_template{file=_}=DT) ->
                                mnesia:delete_object(DT)
                        end,
                        Tmpls)
        end,
           DeleteTemplates)
     end),
    % create new templates:
    admin:get_files_from_folder("templates"),
    admin:get_files_from_folder("templates/internal"),
    admin:get_files_from_folder("templates/analytics"),
    admin:get_files_from_folder("templates/mail"),  % need???
    MFAReplacedTemplates=[["templates/admin.html", "templates/internal/admin.html"],
                        ["templates/login.html", "templates/internal/login.html"],
                        ["templates/main.html", "templates/internal/main.html"],
                        ["templates/navbar.html","templates/internal/navbar.html"],
                        ["templates/setup.html","templates/internal/setup.html"],
                        ["templates/ga_analytics.html","templates/analytics/ga_analytics.html"],
                        ["templates/ya_analytics.html","templates/analytics/ya_analytics.html"],
                        ["templates/hs_analytics.html","templates/analytics/hs_analytics.html"]
                       ],
    transaction(fun() -> % delete old templates
        lists:foreach(
          fun([Old,New]=_TmplPair) ->
              ReplElements = mnesia:match_object(#cms_mfa{mfa={common, template, [Old]}, _='_'}),
              lists:foreach(fun(MFA) ->
                              New_mfa = MFA#cms_mfa{mfa={common, template, [New]},
                                                    updated_at=calendar:universal_time()},
                              mnesia:delete_object(MFA),
                              mnesia:write(New_mfa)
                            end,
                            ReplElements)
          end,
             MFAReplacedTemplates)
       end),
  mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("1.0.1"=VSN) -> % {{{1 : add social-btn images
    admin:get_social_files("static/images"),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("1.0.2"=VSN) -> % {{{1 : add sitemap column for cms_page table
    CurTime = calendar:universal_time(),
    mnesia:transform_table(cms_page, fun({cms_page, Id,D,M,Ar,T,CT,_UT,A,S}) -> 
                                         #cms_page{
                                            id=Id,
                                            description=D,
                                            module=M,
                                            accepted_role=Ar,
                                            title=T,
                                            settings=S,
                                            created_at=CT,
                                            updated_at=CurTime,
                                            active=A,
                                            sitemap=never
                                           }
                                    end, record_info(fields, cms_page)),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("1.0.3"=VSN) -> % {{{1 update submit button
    transaction(
      fun() -> 
        ReplElements = mnesia:match_object(#cms_mfa{mfa={emailform,submit, ['_','_','_']}, _='_'}),
        lists:foreach(fun(#cms_mfa{mfa={M, F, [Block, Email, Classes]}}=MFA) ->
                        New_mfa = MFA#cms_mfa{mfa={M, F, [Block, Email, false, [], Classes]},
                                              updated_at=calendar:universal_time()},
                        mnesia:delete_object(MFA),
                        mnesia:write(New_mfa)
                      end,
                      ReplElements)
      end),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("1.0.4"=VSN) ->  % {{{1
%% update filter for Translation; create language table and fill it
    transaction(
      fun() -> 
        ReplElements = mnesia:match_object(#cms_mfa{settings=#{filters => ['_','_','_']}, _='_'}),
        ?LOG("~nUpdate 1.0.4: ~p blocks ", [length(ReplElements)]),
        lists:foreach(fun(#cms_mfa{settings=#{filters :=  [K,V,R]}}=Item) ->
                        New_item = Item#cms_mfa{settings=#{filters => [K,V,R,[]]},
                                              updated_at=calendar:universal_time()},
                        mnesia:delete_object(Item),
                        mnesia:write(New_item)
                      end,
                      ReplElements)
      end),
    ?CREATE_TABLE(cms_language, set, []),
    Languages_flags = ["flag_en.png","flag_ru.png"],
    Path = "images",
    lists:foreach(fun(Flag_name) ->
                     Asset=admin:file_to_asset(Flag_name, Path),
                     save(Asset)
                  end,
                  Languages_flags),
    AddLangBtn = #{cms_mfa => [admin:add_navbar_button("admin", "sidebar-nav", "languages",
      {{"fa", "globe", ["fab"]}, "Languages"}, {event, ?POSTBACK({admin, languages, show}, admin)})]},
    mnesia:transaction(
      fun() ->
        [maps:map(
           fun(_K, V) ->
                   lists:foreach(
                     fun(#cms_mfa{}=R) ->
                             mnesia:write(
                               fix_sort(
                                 update_timestamps(R)));
                        (R) ->
                             mnesia:write(
                                 update_timestamps(R))
                     end,
                     lists:flatten(V))
           end,
          AddLangBtn )]
      end),
    admin:add_language("ru","flag_ru.png",true),
    admin:add_language("en","flag_en.png",false),
    admin:add_language("any","",false),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("1.0.5"=VSN) -> % update classes for admin elements % {{{1
    mnesia:transaction(
      fun() -> 
        % 1) classes: nav-second-level","collapse" ++ "dropdown-menu
        case mnesia:match_object(#cms_mfa{id={"admin",'_'}, 
                                  mfa={'_','_',['_',["nav-second-level","collapse"]]},
                                  _='_'}) of
            []-> ok;
            L when is_list(L), length(L) > 1 ->
                lists:foreach(fun(#cms_mfa{mfa={M,F,[HtmlId,["nav-second-level","collapse"]=Classes]}}=MFA) ->
                                NewClasses=lists:append(Classes,["dropdown-menu"]),
                                New_mfa = MFA#cms_mfa{mfa={M, F, [HtmlId,NewClasses]},
                                                      updated_at=calendar:universal_time()},
                                mnesia:delete_object(MFA),
                                mnesia:write(New_mfa)
                              end, L)
        end,
        % 2) "arrow" class -> "arrow-right" 
        case mnesia:match_object(#cms_mfa{id={"admin",'_'}, 
                                  mfa={common,icon,['_','_',["arrow"]]},
                                  _='_'}) of
            []-> ok;
            L2 when is_list(L2), length(L2) > 1 ->
                lists:foreach(fun(#cms_mfa{mfa={M,F,[Cl1,Cl2,["arrow"]]}}=MFA) ->
                                NewCl3=["arrow-right"],
                                New_mfa = MFA#cms_mfa{mfa={M, F, [Cl1,Cl2,NewCl3]},
                                                      updated_at=calendar:universal_time()},
                                mnesia:delete_object(MFA),
                                mnesia:write(New_mfa)
                              end, L2)
        end,
        case mnesia:match_object(#cms_asset{id=["js","metisMenu"],
                                  file="css/metisMenu.min.js", _='_'}) of
              [] -> ok;
              [I] when length([I]) == 1 ->
                full_delete(I)
        end
      end),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("1.0.6"=VSN) -> % added google tags element % {{{1
  TemplatePath="templates/analytics/google_tags.html",
  admin:add_template(TemplatePath, "Google tags", TemplatePath, []),
  mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("1.0.7"=VSN) -> % buttons managing db to Pages menu % {{{1
  ManageDbBtns = #{
    cms_mfa => [
      admin:add_navbar_button("admin", "pages-menu", "pages-import",
        {{"fa", "upload", []}, "Import Pages"}, {event, ?POSTBACK({admin, pages, import}, admin)}),
      admin:add_navbar_button("admin", "pages-menu", "pages-export",
        {{"fa", "download", []}, "Export Pages"}, {event, ?POSTBACK({admin, pages, export}, admin)}),
      admin:add_navbar_button("admin", "pages-menu", "pages-merge",
        {{"fa", "code-fork", []}, "Merge Pages"}, {event, ?POSTBACK({admin, pages, merge}, admin)}),
      admin:add_navbar_button("admin", "pages-menu", "db-defragment",
        {{"fa", "th-large", []}, "Defragment db"}, {event, ?POSTBACK({admin, db, defragment}, admin)}),
      admin:add_navbar_button("admin", "pages-menu", "db-clean",
        {{"fa", "trash", []}, "Clear db trash"}, {event, ?POSTBACK({admin, db, clean}, admin)})
    ]},
    mnesia:transaction(
      fun() ->
        [maps:map(
           fun(_K, V) ->
                   lists:foreach(
                     fun(#cms_mfa{}=R) ->
                             mnesia:write(
                               fix_sort(
                                 update_timestamps(R)));
                        (R) ->
                             mnesia:write(
                                 update_timestamps(R))
                     end,
                     lists:flatten(V))
           end,
          ManageDbBtns )]
      end),
  mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("2.0.0"=VSN) -> % admin: added tree of blocks % {{{1
  TreeAssets=[
    #cms_asset{
              id=["js","bootstrap-treeview"],
              name="bootstrap-treeview",
              description="treeview",
              file="js/bootstrap-treeview.min.js",
              minified=true,
              type=script,
              active=true
             },
    #cms_asset{
              id=["css","bootstrap-treeview"],
              name="bootstrap-treeview",
              description="treeview",
              file="css/bootstrap-treeview.min.css",
              minified=true,
              type=css,
              active=true
             }
  ],
  lists:foreach(
    fun(Asset)->
      save(Asset)
    end, TreeAssets
  ),
  TreeStaticBlocks=[
    #cms_mfa{id={"admin", "css"}, mfa={common, asset, [["css", "bootstrap-treeview"]]}},
    #cms_mfa{id={"admin", "script"}, mfa={common, asset, [["js", "bootstrap-treeview"]]}}
  ],
  [ save(fix_sort(update_timestamps(B))) || B <- TreeStaticBlocks],
  ?LOG("~nUpdated to 2.0.0", []),
  mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("2.0.1"=VSN) -> % added index to cms_asset.file {{{1
    mnesia:add_table_index(cms_asset, file),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("2.0.2"=VSN) -> % update old panels {{{1
    mnesia:transaction(
      fun() -> 
        case mnesia:match_object(#cms_mfa{ 
                                  mfa={bootstrap,panel,['_','_','_','_','_']},
                                  _='_'}) of
            []-> ok;
            L ->
                lists:foreach(fun(#cms_mfa{mfa={M,F,[PH,PB,PA,PF,Classes]}}=MFA) ->
                                New_mfa = MFA#cms_mfa{mfa={M, F, [PH,PB,PA,PF,"","","","",Classes,[]]}},
                                mnesia:delete_object(MFA),
                                save(New_mfa)
                              end, L)
        end
      end),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("fix_sort") -> % {{{1
    F = fun() ->
      FoldFun = 
          fun(#cms_mfa{id=ID, sort=Sort, mfa=Mfa}, _Acc) ->
                  case mnesia:match_object(#cms_mfa{id=ID, sort=Sort, active=true, _='_'}) of
                      L when is_list(L), length(L) > 1 ->
                          [
                          if Cur_mfa#cms_mfa.mfa /= Mfa ->
                            Max_sort=find_max_sort(ID),
                            New_mfa = update_record_field(Cur_mfa, sort, Max_sort+1),
                            mnesia:delete_object(Cur_mfa),
                            save(New_mfa);
                          true -> ok
                          end                  
                          || Cur_mfa <- L]; % , Cur_mfa#cms_mfa.sort == Sort, Cur_mfa#cms_mfa.id == ID
                      _ -> ok
                  end,
              ok
          end,
      mnesia:foldl(FoldFun, ok, cms_mfa)
    end,
    {atomic, ok} = mnesia:transaction(F).
    

%% Getters
login(Email, Password) -> % {{{1
    transaction(fun() ->
                        mnesia:match_object(#cms_user{email=Email,
                                                      password=Password,
                                                      _='_'})
                end).

register(Email, Password, Role) -> % {{{1
    register(Email, Password, Role, false).

register(Email, Password, Role, DoConfirm) -> % {{{1
    Users_count = mnesia:table_info('cms_user', size),
    Confirm = if DoConfirm and (Users_count > 0) ->
                     {ok, C} = wf:hex_encode(crypto:strong_rand_bytes(16)),
                     C;
                 true -> 0
              end,
    CT = calendar:universal_time(),
    transaction(fun() ->
                        case mnesia:match_object(#cms_user{email=Email,
                                                           active=true,
                                                           _='_'}) of
                            [] -> 
                                User = #cms_user{
                                          email=Email,
                                          password=Password,
                                          confirm=Confirm,
                                          role=Role,
                                          created_at=CT,
                                          updated_at=CT
                                          },
                                mnesia:write(User),
                                User;
                            _ -> {error, "User already exist"}
                        end
                end).

confirm(undefined) -> % {{{1
    {error, "Wrong or missing confirmation code"};
confirm({Email, Confirm}) -> % {{{1
    transaction(
      fun() ->
              case mnesia:match_object(#cms_user{
                                          email=Email,
                                          confirm=Confirm,
                                          _='_'}) of
                  [User0] ->
                      User = User0#cms_user{confirm=0,
                                            updated_at=calendar:universal_time()},
                      mnesia:write(User),
                      {ok, User};
                  _ -> {error, "User confirmation error"}
              end
      end).

confirm_change_password({Email, OldPassw}, NewPassw) -> % {{{1
    transaction(
      fun() ->
              case mnesia:match_object(#cms_user{
                                          email=Email,
                                          password=OldPassw,
                                          _='_'}) of
                  [User0] ->
                      User = User0#cms_user{password=NewPassw,
                                            updated_at=calendar:universal_time()},
                      mnesia:write(User),
                      {ok, User};
                  _ -> 
                      Mess = "Changing password error",
                      {error, Mess}
              end
      end).

read(Table, Ids) when is_list(Ids) -> % {{{1
    transaction(fun() ->
                        [R || Id <- Ids, R <- mnesia:read(Table, Id)]
                end);
read(Table, Id) -> % {{{1
    read(Table, [Id]).

get_mfa(Page, Block) -> % {{{1
%% @doc "return records list of blocks with id={Page, Block}"
    get_mfa(Page, Block, false).
get_mfa(Page, Block, Replaced) -> % {{{1 
%% @doc "-||- maybe replaced from Page if it has dublicate from {"*", Block} "
    get_mfa(Page, Block, Replaced, false).
get_mfa(Page, Block, Replaced, WithSubBlocks) -> % {{{1 
%% @doc "-||- maybe with subblocks"
    Funs = transaction(
      fun() ->
        G = mnesia:match_object(#cms_mfa{id={"*", Block}, active=true, _='_'}),
        T = mnesia:match_object(#cms_mfa{id={Page, Block}, active=true, _='_'}),
        Subblocks=case WithSubBlocks of 
          true -> mnesia:match_object(#cms_mfa{id={Page, Block++"/link"}, active=true, _='_'})++
                  mnesia:match_object(#cms_mfa{id={Page, Block++"/validate"}, active=true, _='_'});
          false -> []
        end,
        lists:filter(fun(#cms_mfa{sort=S}) -> 
                             not lists:keymember(S, #cms_mfa.sort, T)
                             end, G) ++ Subblocks ++
        case Replaced of
            true ->
                [TE || TE <- T];
            _ ->
                [TE || #cms_mfa{mfa=MFA}=TE <- T, MFA /= undefined]
        end
      end),
    lists:keysort(#cms_mfa.sort, Funs).
get_all_blocks(PID) -> % {{{1
    Blocks = transaction(fun() ->
                                mnesia:select(cms_mfa, 
                                  [{#cms_mfa{id={'$1', '$2'}, active='$3', _='_'},
                                    [{'and', 
                                      {'or', {'==', '$1', PID}, {'==', '$1', "*"}},
                                      {'==', '$3', true}
                                    }],
                                    ['$2']}
                                  ]
                                )
                         end),
    sets:to_list(sets:from_list(Blocks)).
get_parent_block(PID, BID, ActionOrBlock) -> % {{{1
%% @doc "Return parent BlockId if it exists; unactive block without parent"
  transaction(fun() ->
    P1=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_']}, active=true, _='_'}),
    P = 
    case P1 of 
      [] ->
        P2=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_','_']}, active=true, _='_'}),
        case P2 of 
          [] -> P3=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_','_','_']}, active=true, _='_'}),
            case P3 of 
              [] -> P4=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_','_','_','_']}, active=true, _='_'}),
                case P4 of 
                  [] -> P5=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_','_','_','_','_']}, active=true, _='_'}),
                    case P5 of 
                      [] -> AB=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={html5,article,['_',BID,'_','_','_']}, active=true, _='_'}),
                        case AB of 
                          [] -> AF=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={html5,article,['_','_',BID,'_','_']}, active=true, _='_'}),
                            case AF of 
                              [] -> PB=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={bootstrap,panel,['_',BID,'_','_','_','_','_','_','_','_']}, active=true, _='_'}),
                                  case PB of 
                                    [] -> PH=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={bootstrap,panel,[BID,'_','_','_','_','_','_','_','_','_']}, active=true, _='_'}),
                                      case PH of 
                                        [] -> PA=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={bootstrap,panel,['_','_',BID,'_','_','_','_','_','_','_']}, active=true, _='_'}),
                                          case PA of 
                                            [] -> PF=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={bootstrap,panel,['_','_','_',BID,'_','_','_','_','_','_']}, active=true, _='_'}),
                                            case PF of
                                              [] -> P_Dp=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={bootstrap,dropdown,[BID]}, active=true, _='_'}),
                                              case P_Dp of 
                                                [] -> _P_Li=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={html5,list_item,[BID]}, active=true, _='_'});
                                                _ -> P_Dp
                                              end;
                                              _ -> PF
                                            end;
                                            _ -> PA
                                          end;
                                        _ -> PH
                                      end;
                                    _ -> PB
                                  end;
                              _ -> AF
                            end;
                          _ -> AB
                        end;
                      _ -> P5
                    end;
                  _ -> P4
                end;
              _ -> P3
            end;
          _ -> P2
        end;
      _ -> P1
    end,
      
    case P of
      L when is_list(L), length(L) > 1 ->
          length(L);
      [I] ->  
              {_, ParentID}=I#cms_mfa.id,
              ParentID;
      
      _  -> case ActionOrBlock of 
              undefined -> undefined;
              return_if_none -> {none, BID};
              _Some -> delete(ActionOrBlock)
            end

    end
  end).

get_users() -> % {{{1
    transaction(fun() ->
                        Users = mnesia:match_object(#cms_user{active=true, _='_'}),
                        [record_to_map(A) || A <- Users]
                end).

get_user(Email) -> % {{{1
    transaction(fun() ->
                        mnesia:match_object(#cms_user{email=Email, active=true, _='_'}) 
                end).

get_roles() -> % {{{1
    transaction(fun() ->
                        Role = mnesia:match_object(#cms_role{active=true, _='_'}),
                        [record_to_map(A) || A <- Role]
                end).
get_templates() -> % {{{1
    transaction(fun() ->
                        Templates = mnesia:match_object(#cms_template{active=true, _='_'}),
                        [record_to_map(A) || A <- Templates]
                end).

get_template(TID) -> % {{{1
    transaction(fun() ->
                        mnesia:match_object(#cms_template{name=TID, active=true, _='_'}) 
                end).

get_asset(AID) -> % {{{1
    transaction(fun() ->
                        mnesia:match_object(#cms_asset{id=AID, active=true, _='_'}) 
                end).

get_assets(Type) -> % {{{1
    transaction(fun() ->
                        Assets = mnesia:select(cms_asset, [{#cms_asset{type=Type, active=true, _='_'}, [], ['$_']}]),
                        [record_to_map(A) || A <- Assets]
                end).

get_forms() -> % {{{1
    transaction(fun() ->
                        Forms = mnesia:match_object(#cms_form{active=true, _='_'}),
                        [record_to_map(A) || A <- Forms]
                end).

get_languages() -> % {{{1
    transaction(fun() ->
                        Langs = mnesia:match_object(#cms_language{_='_'}),
                        [record_to_map(A) || A <- Langs]
                end).
get_language(Lid) -> % {{{1
    transaction(fun() ->
                        mnesia:match_object(#cms_language{id=Lid,_='_'})
                end).

fix_sort(Recs) when is_list(Recs) -> % {{{1
    [fix_sort(Rec) || Rec <- Recs];
fix_sort(#cms_mfa{sort=new}=Rec) -> % {{{1
    fix_sort(Rec#cms_mfa{sort=1});
fix_sort(#cms_mfa{id={PID, Block}, sort=Sort0}=Rec) -> % {{{1
    Sort = case get_mfa(PID, Block, true) of
               [] -> 0;
               MFAs ->
                   #cms_mfa{sort=S} = lists:last(
                                        lists:keysort(#cms_mfa.sort, MFAs)),
                   S
           end,
    Rec#cms_mfa{sort=Sort+Sort0}.

update(OldRecord, NewRecord) -> % {{{1
    transaction(fun() ->
                        mnesia:delete_object(OldRecord),
                        UR = update_record_field(NewRecord, updated_at, calendar:universal_time()),
                        mnesia:write(UR)
                end).

update(Record, Field, Value) -> % {{{1
    transaction(fun() ->
                        mnesia:delete_object(Record),
                        R1 = update_record_field(Record, Field, Value),
                        UR = update_record_field(R1, updated_at, calendar:universal_time()),
                        mnesia:write(UR),
                        Value
                end).

copy_page(#{id := PID}= Map) -> % {{{1
    NewPID = "copy_" ++ PID,
    NewMap = maps:update(id, NewPID, Map),
    update_map(NewMap),
    transaction(fun() ->
                    case mnesia:match_object(#cms_mfa{id={PID, '_'}, _='_'}) of
                        [] ->
                            ok;
                        L -> 
                            lists:foreach(fun(#cms_mfa{id={_, Block}}=DbItem) ->
                                NewDbItem = update_record_field(DbItem, id, {NewPID, Block}),
                                UR = update_record_field(NewDbItem, updated_at, calendar:universal_time()),
                                mnesia:write(UR)
                                end, L)
                    end
                end).

update_map(Map) -> % {{{1
    update_map(Map, fun fields/1).
    
update_map(Map, FieldsFun) -> % {{{1
    save(map_to_record(Map, FieldsFun)).

update_language(#{id := Id, icon:= Icon, default:=Default}) -> % {{{1
    transaction(
      fun() ->
        case mnesia:match_object(#cms_language{id=Id, _='_'}) of
          [] -> ok;
          [DbItem] ->  
            NewDbItem1 = update_record_field(DbItem, icon, Icon),
            NewDbItem2 = update_record_field(NewDbItem1, default, Default),
            mnesia:delete_object(DbItem),
            mnesia:write(NewDbItem2)
        end
      end).

rename_page(#{id := NewPID, old_value := OldValue} = Map) ->  % {{{1
    update_map(Map),
    transaction(fun() ->
                    case mnesia:match_object(#cms_mfa{id={OldValue, '_'}, _='_'}) of
                        [] ->
                            ok;
                        L -> 
                            lists:foreach(fun(#cms_mfa{id={_, Block}}=DbItem) ->
                                NewDbItem = update_record_field(DbItem, id, {NewPID, Block}),
                                mnesia:write(NewDbItem),
                                mnesia:delete_object(DbItem)
                                end, L)
                    end,
                    OldPages = mnesia:match_object(#cms_page{id=OldValue, _='_'}),
                    [mnesia:delete_object(O) || O <- OldPages]
                end).
                        
get_pages() -> % {{{1
    transaction(fun() ->
                        Pages = mnesia:match_object(#cms_page{active=true, _='_'}),
                         [record_to_map(A) || A <- Pages]
                end).

get_page(PID) -> % {{{1
    transaction(fun() ->
                        mnesia:match_object(#cms_page{id=PID, active=true, _='_'})
                end).

save([]) -> % {{{1
    [];
save([Record|T]) -> % {{{1
    [save(Record) | save(T)];
save(Record0) -> % {{{1
    Record = update_timestamps(Record0),
    io:format("~nSave: ~p~n", [Record]),
    transaction(fun() ->
                        mnesia:write(Record),
                        Record
                end).

maybe_save(Assets) when is_list(Assets) -> % {{{1
    lists:foreach(fun(A) -> maybe_save(A) end, Assets);
maybe_save(#cms_asset{file=Path}=Asset) -> % {{{1
    transaction(fun() ->
                        case mnesia:index_read(cms_asset, Path, #cms_asset.file) of
                            [] -> mnesia:write(Asset);
                            _ -> ok
                        end
                end).
maybe_delete(#cms_mfa{id={PID, Block}, sort=Sort}) -> % {{{1
    transaction(fun() ->
                        case mnesia:match_object(#cms_mfa{id={PID, Block}, sort=Sort, active=true, _='_'}) of
                            [] ->
                                case Sort of
                                  new -> [];
                                  _ -> mnesia:write(empty_mfa(PID, Block, Sort))
                                end;
                            L when is_list(L) -> 
                              lists:foreach(fun(DbItem) ->
                                UR = update_record_field(DbItem, updated_at, calendar:universal_time()),
                                DR = update_record_field(UR, active, false),
                                io:format("~nUnactiveItem: ~p", [DR]),
                                mnesia:delete_object(DbItem),
                                mnesia:write(DR)
                                end, L)
                                % [mnesia:delete_object(B1) || B1 <- L, B1#cms_mfa.sort == Sort]
                        end
                end).
maybe_update(#cms_mfa{id={PID, Block}, sort=Sort}=B) -> % {{{1
    transaction(fun() ->
                        case mnesia:wread({cms_mfa, {PID, Block}}) of
                            [] ->
                                ok;
                            L when is_list(L) -> 
                                [mnesia:delete_object(B1) || B1 <- L, B1#cms_mfa.sort == Sort]
                        end,
                        UR = update_record_field(B, updated_at, calendar:universal_time()),
                        mnesia:write(UR)
                end).

delete(#{}=Map) -> % {{{1
    delete(Map, fun fields/1);
delete(#cms_page{id=PID}=Record) ->
            transaction(fun() ->
                        case mnesia:match_object(#cms_mfa{id={PID, _Block='_'}, _='_'}) of
                            [] ->
                                ok;
                            L when is_list(L) -> 
                            %     lists:foreach(fun(DbItem) ->
                            %       delete(DbItem)
                            %       end, L)
                                [full_delete(B1) || B1 <- L] % full_delete
                        end,
                        UP = update_record_field(Record, updated_at, calendar:universal_time()),
                        DP = update_record_field(UP, active, false),
                        mnesia:delete_object(Record),
                        mnesia:write(DP)
            end),
            io:format("Delete page: ~p:~n", [Record]);
% delete(#cms_mfa{sort=new}=Record) -> % {{{1
%     full_delete(Record);
delete(Record) -> % {{{1
    io:format("~nUnactive record: ~p~n", [Record]),
    transaction(fun() ->
                        UP = update_record_field(Record, updated_at, calendar:universal_time()),
                        DP = update_record_field(UP, active, false),
                        mnesia:delete_object(Record),
                        mnesia:write(DP)
                end).

%% For convenience in install and update process
delete(#{}=Map, FieldsFun) -> % {{{1
    delete(map_to_record(Map, FieldsFun)).

%% Delete item from DB
full_delete(#{}=Map) -> % {{{1
    full_delete_map(Map, fun fields/1);
full_delete(Record) -> % {{{1 
    io:format("~nFull_delete: ~p~n", [Record]),
    transaction(fun() ->
                        mnesia:delete_object(Record)
                end).
full_delete_map(#{}=Map, FieldsFun) -> % {{{1
    full_delete(map_to_record(Map, FieldsFun)).

verify_create_table({atomic, ok}) -> ok; % {{{1
verify_create_table({aborted, {already_exists, _Table}}) -> ok. % {{{1

% Wrapper around Mnesia transaction which returns empty list on errors
transaction(F) -> % {{{1
    case mnesia:transaction(F) of
        {atomic, R} -> R;
        _ -> []
    end.
                              
update_record_field(Record, Field, Value) -> % {{{1
    [Rec|RecList] = tuple_to_list(Record),
    Fields = mnesia:table_info(Rec, attributes),
    Map = maps:from_list(lists:zip(Fields, RecList)),
    NewMap = maps:update(Field, Value, Map),
    % Updated_at_Map = maps:update(updated_at, calendar:universal_time(), NewMap),
    NewList = [maps:get(K, NewMap, undefined) || K <- Fields],
    list_to_tuple([Rec|NewList]).

record_to_map(Record) -> % {{{1
    record_to_map(Record, fun fields/1).

record_to_map(Record, FieldsFun) -> % {{{1
    RecList = tuple_to_list(Record),
    Fields = FieldsFun(hd(RecList)),
    RFields = [record | Fields],
    Map = maps:from_list(lists:zip(RFields, RecList)),
    Type = mnesia:table_info(maps:get(record, Map), type),
    Map#{table_type => Type}.

map_to_record(Map) -> % {{{1
    map_to_record(Map, fun fields/1).

map_to_record(#{record := Rec}=Map, FieldsFun) -> % {{{1
    Fields = FieldsFun(Rec),
    RFields = [record | Fields],
    NewList = [maps:get(K, Map, undefined) || K <- RFields],
    list_to_tuple(NewList).

fields(cms_asset) -> % {{{1
    record_info(fields, cms_asset);
fields(cms_page) -> % {{{1
    record_info(fields, cms_page);
fields(cms_mfa) -> % {{{1
    record_info(fields, cms_mfa);
fields(cms_user) -> % {{{1
    record_info(fields, cms_user);
fields(cms_role) -> % {{{1
    record_info(fields, cms_role);
fields(cms_template) -> % {{{1
    record_info(fields, cms_template);
fields(cms_form) -> % {{{1
    record_info(fields, cms_form);
fields(cms_language) -> % {{{1
    record_info(fields, cms_language).

empty_mfa(PID, Block, Sort) -> % {{{1
    CT = calendar:universal_time(),
    #cms_mfa{
       id={PID, Block},
       sort=Sort,
       mfa=undefined,
       created_at=CT,
       updated_at=CT
       }.

get_db_vsn() -> % {{{1
    transaction(fun() ->
                        [#cms_settings{value=VSN}|_] = mnesia:read(cms_settings, vsn),
                        VSN
                end).

merge_backup_and_db(Source, Mod) -> % {{{1
    View = fun(Item, Acc) ->
                [RecType|_RecList] = tuple_to_list(Item),
                case RecType of
                    cms_mfa ->
                        Id=Item#cms_mfa.id,
                        Sort=Item#cms_mfa.sort,
                        transaction(fun() ->
                            case mnesia:match_object(#cms_mfa{id=Id, sort=Sort, _='_'}) of
                                [] ->
                                    mnesia:write(Item),
                                    io:format("~nNew item: ~p",[Item]);
                                L when is_list(L) -> 
                                    [
                                      if Item#cms_mfa.updated_at>DbItem#cms_mfa.updated_at -> 
                                        mnesia:delete_object(DbItem),
                                        mnesia:write(Item),
                                        io:format("~nupdate from:~p~n   to ~p",[DbItem,Item])
                                      end ||  DbItem <- L
                                    ]
                            end
                        end);
                    cms_asset ->
                        Id=Item#cms_asset.id,
                        File=Item#cms_asset.file,
                        transaction(fun() ->
                            case mnesia:match_object(#cms_asset{id=Id, file=File,_='_'}) of
                                [] ->
                                    mnesia:write(Item),
                                    io:format("~nNew item: ~p",[Item]);
                                L when is_list(L) -> 
                                    [
                                      if (Item#cms_asset.updated_at>DbItem#cms_asset.updated_at)  -> 
                                        mnesia:delete_object(DbItem),
                                        mnesia:write(Item),
                                        io:format("~nupdate from:~p~n       to ~p",[DbItem,Item])
                                      end ||  DbItem <- L
                                    ]
                            end
                        end);
                    cms_page ->
                        Id=Item#cms_page.id,
                        transaction(fun() ->
                            case mnesia:match_object(#cms_page{id=Id,_='_'}) of
                                [] ->
                                    mnesia:write(Item),
                                    io:format("~nNew item: ~p",[Item]);
                                L when is_list(L) -> 
                                    [
                                      if (Item#cms_page.updated_at>DbItem#cms_page.updated_at)  -> 
                                        mnesia:delete_object(DbItem),
                                        mnesia:write(Item),
                                        io:format("~nupdate from:~p~n       to ~p",[DbItem,Item])
                                      end ||  DbItem <- L
                                    ]
                            end
                        end);
                    cms_template ->
                        Name=Item#cms_template.name,
                        transaction(fun() ->
                            case mnesia:match_object(#cms_template{name=Name,_='_'}) of
                                [] ->
                                    mnesia:write(Item),
                                    io:format("~nNew item: ~p",[Item]);
                                L when is_list(L) -> 
                                    [
                                      if (Item#cms_template.updated_at>DbItem#cms_template.updated_at)  -> 
                                        mnesia:delete_object(DbItem),
                                        mnesia:write(Item),
                                        io:format("~nupdate from:~p~n       to ~p",[DbItem,Item])
                                      end ||  DbItem <- L
                                    ]
                            end
                        end);
                    _Else -> false          
                end,

                {[Item], Acc + 1}
           end,
    mnesia:traverse_backup(Source, Mod, dummy, read_only, View, 0).

update_timestamps(Recs) when is_list(Recs) -> % {{{
    [update_timestamps(Rec) || Rec <- Recs];
update_timestamps(#cms_mfa{created_at=undefined}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_mfa{created_at=CT, updated_at=CT};
update_timestamps(#cms_page{created_at=undefined}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_page{created_at=CT, updated_at=CT};
update_timestamps(#cms_form{created_at=undefined}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_form{created_at=CT, updated_at=CT};
update_timestamps(#cms_user{created_at=undefined}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_user{created_at=CT, updated_at=CT};
update_timestamps(#cms_role{created_at=undefined}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_role{created_at=CT, updated_at=CT};
update_timestamps(#cms_asset{created_at=undefined}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_asset{created_at=CT, updated_at=CT};
update_timestamps(#cms_template{created_at=undefined}=Rec) ->
    CT = calendar:universal_time(),
    Rec#cms_template{created_at=CT, updated_at=CT};
update_timestamps(#cms_mfa{}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_mfa{updated_at=CT};
update_timestamps(#cms_page{}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_page{updated_at=CT};
update_timestamps(#cms_form{}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_form{updated_at=CT};
update_timestamps(#cms_user{}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_user{updated_at=CT};
update_timestamps(#cms_role{}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_role{updated_at=CT};
update_timestamps(#cms_asset{}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_asset{updated_at=CT};
update_timestamps(#cms_template{}=Rec) -> % {{{1
    CT = calendar:universal_time(),
    Rec#cms_template{updated_at=CT}.

datetime_tostr(Date) -> % {{{1
    {{Year, Month, Day}, {Hour, Minute, Second}} = Date,
    _StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w+00:00",[Year,Month,Day,Hour,Minute,Second])).

clear_page_by_id(PID)-> % {{{1
  transaction(fun() -> 
    Elements = mnesia:match_object(#cms_mfa{id={PID,'_'}, _='_'}),
    lists:foreach(fun(MFA) ->
                          full_delete(MFA)
                  end, Elements)
  end).

%% get pages where sitemap is not none
get_indexed_pages() -> % {{{1
    transaction(fun() ->
                      L = mnesia:match_object(#cms_page{active=true,  _='_'}),
                      lists:filter(fun(#cms_page{sitemap=S}) -> 
                                             S/=none
                                        end, L)
                end).

remove_old_unused_blocks() -> % {{{1
%% @doc "Remove blocks from db: if its have new analog block"
  transaction(
    fun() ->
        Unactive = mnesia:match_object(#cms_mfa{active=false,  _='_'}),
        lists:foreach(fun(#cms_mfa{id=Id,mfa={_M, _F, A}}=MbDelete) ->
          Args = case A of
            [BlockId] -> [BlockId];
            [BlockId, _A2] ->  [BlockId, '_'];
            [BlockId, _A2, _A3] -> [BlockId, '_', '_'];
            [BlockId, _A2, _A3, _A4] -> [BlockId, '_', '_', '_'];
            [BlockId, _A2, _A3, _A4, _A5] -> [BlockId, '_', '_', '_', '_'];
            [BlockId, _A2, _A3, _A4, _A5, _A6] -> [BlockId, '_', '_', '_', '_', '_'];
            [BlockId, _A2, _A3, _A4, _A5, _A6, _A7] -> [BlockId, '_', '_', '_', '_', '_', '_'];
            [BlockId, _A2, _A3, _A4, _A5, _A6, _A7, _A8] -> [BlockId, '_', '_', '_', '_', '_', '_', '_'];
            [BlockId, _A2, _A3, _A4, _A5, _A6, _A7, _A8, _A9] -> [BlockId, '_', '_', '_', '_', '_', '_', '_', '_'];
            [BlockId, _A2, _A3, _A4, _A5, _A6, _A7, _A8, _A9, _A10] -> [BlockId, '_', '_', '_', '_', '_', '_', '_', '_', '_'];
            _ -> A
          end,
          case mnesia:match_object(#cms_mfa{id=Id, active=true,mfa={'_', '_', Args},  _='_'}) of
            [] ->
                undefined;
            _Item -> 
                full_delete(MbDelete)
          end
        end, Unactive)
    end
  ),
  wf:wire(#alert{ text="Trash was cleared!"}).

remove_blocks_without_parent() -> % {{{1
%% @doc "Unactive all blocks if its havent parent"
  transaction(
      fun() ->
        AllBlocks = mnesia:match_object(#cms_mfa{_ = '_', active=true}),
        Exclude_blocks = [css,body,page,router,script],
        Exclude_pages = [admin,login,restore,register],
        lists:foreach(fun(#cms_mfa{id={PID,BID}}=Block) ->
          NotSubblock= 
            case re:run(BID,"^\\+|\\/",[global]) of
              nomatch -> true;
              _ -> false
            end,
          Condition = not lists:member(wf:to_atom(BID), Exclude_blocks) and NotSubblock
            and not lists:member(wf:to_atom(PID), Exclude_pages),
          case Condition of
            false -> undefined;
            _-> get_parent_block(PID,BID,Block)
            
          end
        end, AllBlocks)
      end
  ),
  wf:wire(#alert{ text="Database was defragmented!"}). 

get_blocks_without_parent(PID) -> % {{{1
%% @doc "Return list of block names which havent parent"
  AllBlocks = transaction(
      fun() ->
        PageBlocks=mnesia:match_object(
                #cms_mfa{id={PID,'_'},
                         active=true,
                         _='_'}
        ),
        case PID of
            "*" -> PageBlocks;
            _->
              GlobalBlocks=mnesia:match_object(
                #cms_mfa{id={"*",'_'},
                         active=true,
                         _='_'}
              ),
              
              FGb=lists:filter(
                fun(#cms_mfa{id={_,Bid}, sort=S}) -> 
                  case mnesia:match_object(#cms_mfa{id={PID,Bid},sort=S,active=true,_='_'}) of
                    [] -> true;
                    _ -> ?LOG("  Replaced block: {\"*\",~p},sort=~p on Page:~p", [Bid,S,PID]),
                        false
                  end
                end, GlobalBlocks),
              FGb ++ PageBlocks
        end
      end
  ),
  ListBlocks=lists:filtermap(
          fun(#cms_mfa{id={PID0,Bid0}}) ->
            BID=case re:run(Bid0, "(\\S+)/(link|validate)$") of
                    {match,[{_,_},{StartPos,Len},{_,_}]} ->
                      string:substr(Bid0, StartPos+1, Len);
                    nomatch -> Bid0
                  end,
            case get_parent_block(PID0,BID,return_if_none) of
              {none, BID} -> {true,BID};
              _-> false
            end
          end, AllBlocks),
  ListWithBody=lists:append(["body"],ListBlocks),
  Set = sets:from_list(ListWithBody),
  sets:to_list(Set).

find_max_sort({PID,Block}) -> % {{{1
%% @doc "return max sort among choosen id of cms_mfa"
  transaction(
    fun() ->
      Blocks=mnesia:match_object(#cms_mfa{id={PID,Block},active=true, _='_'}),
      Sorts=[S || #cms_mfa{sort=S} <-Blocks],
      _Max_sort = lists:max(Sorts)
    end
  ).

update_children(PID, NewBlockId, OldBlockId) -> % {{{1
%% @doc "update children of Block by old Block Id"
  if (NewBlockId/=undefined) and (OldBlockId/=undefined) ->
    transaction(
      fun() ->
        OldChldrenBlocks=mnesia:match_object(#cms_mfa{id={PID,OldBlockId},active=true, _='_'}),
        lists:foreach(
          fun(Block) ->
              NewBlock=Block#cms_mfa{id={PID, NewBlockId},
                                   updated_at=calendar:universal_time()},
              full_delete(Block),
              mnesia:write(NewBlock)
          end, 
        OldChldrenBlocks)
      end
    );
  true->ok
  end.

extract_mfa_block_name(#cms_mfa{mfa=MFA}) -> % {{{1
%% @doc "extract block name from mfa if possible"
  {_M,F,Args}=admin:parse_mfa_field(MFA),
  Exclude_functions=[text,template,img,asset,undefined],
  HasBlockName = is_list(Args) and (length(Args) > 0) and not lists:member(wf:to_atom(F), Exclude_functions),
  case HasBlockName of
    true  ->
      case F of
        panel ->
          [_Ph,Pb|_]=Args,
          Pb;
        article ->
          [_Ah,Ab|_]=Args,
          Ab;
        _ ->[H|_]=Args,
            H
      end;
    _ -> undefined
  end.
