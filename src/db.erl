-module(db).
%%%
%%% Mnesia DB backend routines module.
%%% Put your backend code here
%%%

-compile([export_all]).
-include("db.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%% Don't remove! This is is used to install your Mnesia DB backend  from CLI tool
install([])-> % {{{1
    ?CREATE_TABLE(cms_settings, set, []),
    ?CREATE_TABLE(cms_mfa, bag, []),
    ?CREATE_TABLE(cms_template, set, []),
    ?CREATE_TABLE(cms_asset, bag, [type, name]),
    ?CREATE_TABLE(cms_page, set, []),
    ?CREATE_TABLE(cms_user, set, []),
    ?CREATE_TABLE(cms_role, set, []),
    ?CREATE_TABLE(cms_form, set, []),
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
    mnesia:backup("mnesia.lcms"),
    mnesia:delete_table(cms_user),
    ?CREATE_TABLE(cms_user, set, []),
    mnesia:restore("mnesia.lcms", []),
    mnesia:dirty_write(#cms_settings{key=vsn, value=VSN});
update("0.1.2"=VSN) -> % {{{1
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
    db:save(AdminPage#cms_page{module=index, updated_at=calendar:universal_time()}),

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
                                                  % io:format("~nReplace ~p to ~p",[MFA, New_mfa])
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
                              % ,io:format("~nReplace ~p to ~p",[MFA, New_mfa])
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
update("fix_sort") -> % {{{1
    F = fun() ->
      FoldFun = 
          fun(#cms_mfa{id=ID, sort=Sort, mfa=Mfa}, _Acc) ->
                  case mnesia:match_object(#cms_mfa{id=ID, sort=Sort, _='_'}) of
                      [] ->
                          ok;
                      L when is_list(L), length(L) > 1 ->
                          [
                          if Cur_mfa#cms_mfa.mfa /= Mfa ->
                            New_sort = Sort -1 + string:str(L, [Cur_mfa]),
                            %io:format("~nold_sort ~p,new_sort: ~p", [Sort, New_sort]),
                            New_mfa = update_record_field(Cur_mfa, sort, New_sort),
                            %io:format("~nupdate [~p]~p", [Sort, New_mfa]),
                            mnesia:delete_object(Cur_mfa),
                            mnesia:write(New_mfa);
                          true -> ok
                          end                          
                          || Cur_mfa <- L, Cur_mfa#cms_mfa.sort == Sort, Cur_mfa#cms_mfa.id == ID];
                      _Else -> ok
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
    get_mfa(Page, Block, false).
get_mfa(Page, Block, Replaced) -> % {{{1 
    Funs = transaction(fun() ->
                        G = mnesia:read(cms_mfa, {"*", Block}),
                        T = mnesia:read(cms_mfa, {Page, Block}),
                        lists:filter(fun(#cms_mfa{sort=S}) -> 
                                             not lists:keymember(S, #cms_mfa.sort, T)
                                             end, G) ++ 
                        case Replaced of
                            true ->
                                [TE || #cms_mfa{active=Active}=TE <- T, Active == true];
                            _ ->
                                [TE || #cms_mfa{mfa=MFA, active=Active}=TE <- T, MFA /= undefined, Active == true]
                        end
                            
                end),
    % io:format("~nget_mfa ~p",[lists:keysort(#cms_mfa.sort, Funs)]),
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
    % io:format("~n_block~p",[sets:from_list(Blocks)]),
    sets:to_list(sets:from_list(Blocks)).
get_parent_block(PID, BID) -> % {{{1
    transaction(fun() ->
                        P1=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_']}, active=true, _='_'}),
                        P2=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_','_']}, active=true, _='_'}),
                        P3=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_','_','_']}, active=true, _='_'}),
                        P4=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_','_','_','_']}, active=true, _='_'}),
                        P5=mnesia:match_object(#cms_mfa{id={PID,'_'},mfa={'_','_',[BID,'_','_','_','_','_']}, active=true, _='_'}),
                        P12=lists:append(P1,P2),
                        P34=lists:append(P3,P4),
                        P35=lists:append(P34,P5),
                        P=lists:append(P12,P35),
                        case P of
                          L when is_list(L), length(L) > 1 ->
                              length(L);
                          [I] ->  
                                  {_, ParentID}=I#cms_mfa.id,
                                  ParentID;
                          
                          _  ->  undefined
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

fix_sort(Recs) when is_list(Recs) -> % {{{1
    [fix_sort(Rec) || Rec <- Recs];
fix_sort(#cms_mfa{sort=new}=Rec) -> % {{{1
    fix_sort(Rec#cms_mfa{sort=1});
fix_sort(#cms_mfa{id={PID, Block}, sort=Sort0}=Rec) -> % {{{1
    Sort = case get_mfa(PID, Block) of
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
    % io:format("Db update: ~p~n", [Record]),
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
    %io:format("~nDb copy PID: ~p to ~p~n", [PID, NewPID]),
    update_map(NewMap),
    transaction(fun() ->
                    case mnesia:match_object(#cms_mfa{id={PID, '_'}, _='_'}) of
                        [] ->
                            ok;
                        L -> 
                            lists:foreach(fun(#cms_mfa{id={_, Block}}=DbItem) ->
                                NewDbItem = update_record_field(DbItem, id, {NewPID, Block}),
                                % io:format("~nCopy NewDbItem: ~p~n", [NewDbItem]),
                                mnesia:write(NewDbItem)
                                end, L)
                    end
                end).

update_map(Map) -> % {{{1
    update_map(Map, fun fields/1).
    
update_map(Map, FieldsFun) -> % {{{1
    io:format("~nDb update_map:", []),
    save(map_to_record(Map, FieldsFun)).

rename_page(#{id := NewPID, old_value := OldValue} = Map) ->  % {{{1
    update_map(Map),
    %io:format("~nDb rename_page: ~p~n~p", [OldValue,Map]),
    transaction(fun() ->
                    case mnesia:match_object(#cms_mfa{id={OldValue, '_'}, _='_'}) of
                        [] ->
                            ok;
                        L -> 
                            lists:foreach(fun(#cms_mfa{id={_, Block}}=DbItem) ->
                                NewDbItem = update_record_field(DbItem, id, {NewPID, Block}),
                                % io:format("~nCopy NewDbItem: ~p~n", [NewDbItem]),
                                mnesia:write(NewDbItem),
                                mnesia:delete_object(DbItem)
                                end, L)
                    end,
                    OldPages = mnesia:match_object(#cms_page{id=OldValue, _='_'}),
                    % io:format("~ndelete_object: ~p", [OldPage ]),
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

maybe_delete(#cms_mfa{id={PID, Block}, sort=Sort}) -> % {{{1
    transaction(fun() ->
                        case mnesia:match_object(#cms_mfa{id={PID, Block}, sort=Sort, active=true, _='_'}) of
                            [] ->
                                mnesia:write(empty_mfa(PID, Block, Sort));
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
delete(#cms_page{id=PID}=Record) -> % {{{1
            transaction(fun() ->
                        case mnesia:match_object(#cms_mfa{id={PID, '_'}, _='_'}) of
                            [] ->
                                ok;
                            L when is_list(L) -> 
                                lists:foreach(fun(DbItem) ->
                                  UR = update_record_field(DbItem, updated_at, calendar:universal_time()),
                                  DR = update_record_field(UR, active, false),
                                  mnesia:delete_object(DbItem),
                                  mnesia:write(DR)
                                  end, L)
                                % [mnesia:delete_object(B1) || B1 <- L] % full_delete
                        end,
                        UP = update_record_field(Record, updated_at, calendar:universal_time()),
                        DP = update_record_field(UP, active, false),
                        io:format("Deleted page: ~p:~n", [DP]),
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
full_delete(Record) -> % {{{1 
    io:format("~nFull_delete: ~p~n", [Record]),
    transaction(fun() ->
                        mnesia:delete_object(Record)
                end).
%% For convenience in install and update process
delete(#{}=Map, FieldsFun) -> % {{{1
    % io:format("~nDelete map: ~p~n", [Map]),
    delete(map_to_record(Map, FieldsFun)).

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
    record_info(fields, cms_form).

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

datetime_tostr(Date) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = Date,
    _StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w+00:00",[Year,Month,Day,Hour,Minute,Second])).
