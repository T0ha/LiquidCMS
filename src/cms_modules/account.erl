%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (account).
-compile([export_all, {parse_transform, lager_transform}]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

%% CMS Module interface {{{1
description() -> % {{{2
    "Account".

functions() -> % {{{2
    [
     {email_field, "Login /Email Field"},
     {password_field, "Password Field"},
     {retype_password_field, "Retype Password Field"},
     {login_button, "Login Button"},
     {logout_button, "Logout Button"},
     {register_button, "Register Button"}
     ].

format_block(F, [Block|_]=A) -> % {{{2
    {wf:f("account:~s(~p)", [F, A]), Block}.

form_data(register_button, A) -> % {{{2
    [_, Block, Role, Classes] = admin:maybe_empty(A, 4),

    {[ 
      {"Role",
       #dd{
          id=register_role,
          value=admin:remove_prefix(Role),
          options=admin:cms_roles()
         }
      }
     ],
     [],
     Block,
     Classes
    }.

save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, col, [Block, Role, Classes]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={bootstrap, col, [Block, wf:to_atom(Role), Classes]}}.

%% Module render functions {{{1
main() -> % {{{2
    PID = case wf:q(page) of
              undefined -> "login";
              A -> A
          end,
    Page = case db:get_page(PID) of
               [P] -> P;
               [] -> #cms_page{id="404"}
           end, 
            
    common:waterfall(Page, "page").

title() -> "LiquidCMS - Log In".

body(Page) ->  % {{{2
    index:body(Page).
	
email_field(Page) -> % {{{2
    email_field(Page, "", "").

password_field(Page) -> % {{{2
    password_field(Page, "", "").

retype_password_field(Page) -> % {{{2
    retype_password_field(Page, "", "").

login_button(Page) -> % {{{2
    login_button(Page, "", "").

register_button(Page, Role) -> % {{{2
    register_button(Page, "", Role, "").

email_field(_Page, _Block, Classes) -> % {{{2
     #panel{
        class="form-group",
        body=#txtbx{
                id=email,
                class=Classes,
                placeholder="E-mail"}}.

password_field(_Page, _Block, Classes) -> % {{{2
    #panel{
       class="form-group",
       body=#pass{
               id=password,
               class=Classes,
               placeholder="Password"}}.

retype_password_field(_Page, _Block, Classes) -> % {{{2
     #panel{
        class="form-group",
        body=#pass{
                id=repassword,
                class=Classes,
                actions=#validate{
                           validators=#confirm_password{
                                         text="Password and confirmation are different",
                                         password=password
                                        }
                          },
                placeholder="Confirm Password"}}.

login_button(Page, Block, Classes) -> % {{{2
     #btn{
        type=success,
        size=lg,
        class=["btn-block" | Classes],
        text=common:parallel_block(Page, Block),
        postback={auth, login},
        delegate=?MODULE
       }.

logout_button(Page, Block, Classes) -> % {{{2
    logout_button(Page, Block, link, "", Classes).

logout_button(Page, Block, Type, Size, Classes) -> % {{{2
     #btn{
        type=Type,
        size=Size,
        class=[Classes],
        text=common:parallel_block(Page, Block),
        postback={auth, logout},
        delegate=?MODULE
       }.

register_button(_Page, _Block, Role, Classes) -> % {{{2
    #btn{
       type=success,
       size=lg,
       class=["btn-block" | Classes],
       text="Register",
       postback={auth, register, wf:to_atom(Role)},
       delegate=?MODULE
      }.

login_form(Page, _Block, _Classes) -> % {{{2
    login_form(Page).

login_form(Page) -> % {{{2
    [
     email_field(Page),
     password_field(Page),
     login_button(Page)
    ].

register_form(Page, _Block, Role, _Classes) -> % {{{2
    register_form(Page, Role).

register_form(Page, Role) -> % {{{2
    [
     email_field(Page),
     password_field(Page),
     retype_password_field(Page),
     register_button(Page, Role)
    ].
maybe_redirect_to_login(#cms_page{accepted_role=undefined} = Page) -> % {{{2
    maybe_redirect_to_login(Page#cms_page{accepted_role=nobody});
maybe_redirect_to_login(#cms_page{accepted_role=nobody} = Page) -> % {{{2
    wf:info("Not redirect to login: ~p", [Page]),
    Page;
maybe_redirect_to_login(#cms_page{accepted_role=Role} = Page) -> % {{{2
    wf:info("Redirect to login: ~p", [Page]),
    case wf:role(Role) of 
        true ->
            Page;
        false ->
            wf:redirect_to_login("/account")
    end.
%% Module install routines {{{1
default_data() -> % {{{2
    #{
     cms_role => [
                  #cms_role{role = nobody, sort=?ROLE_NOBODY_SORT, name="Nobody"},
                  #cms_role{role = admin, sort=?ROLE_ADMIN_SORT, name="Admin"},
                  #cms_role{role = root, sort=?ROLE_ROOT_SORT, name="Root"},
                  #cms_role{role = editor, sort=?ROLE_EDITOR_SORT, name="Editor"}
                 ]}.

install() -> % {{{2
    lager:info("Installing ~p module", [?MODULE]),
    % Log In page
    admin:add_page("login", "templates/login.html", undefined, account),
    admin:add_to_block("login", "css", {asset, ["css", "sb-admin-2"]}, 3),

    % Index Setup page
    admin:add_page("index", "templates/setup.html", undefined, index),
    admin:add_to_block("index", "router", {router, page, ["register"]}, 1),

    % Register page
    admin:add_page("register", "templates/setup.html", undefined, index),

    admin:add_to_block("register", "admin-setup", {bootstrap, col, ["col-admin", "4", "4", ""]}, 1),
    admin:add_to_block("register", "col-admin", {bootstrap, panel, ["admin-panel-header", "admin-panel-body", "", "", ["panel-default"]]}, 1),
    admin:add_to_block("register", "admin-panel-header", {text, ["Admin Account Settings"]}, 1),
    admin:add_to_block("register", "admin-panel-body", {account, email_field, []}, 1),
    admin:add_to_block("register", "admin-panel-body", {account, password_field, []}, 2),
    admin:add_to_block("register", "admin-panel-body", {account, retype_password_field, []}, 3),
    admin:add_to_block("register", "admin-panel-body", {account, register_button, [admin]}, 4),

    ok.

%% Event handlers {{{1
event({auth, register}) -> % {{{2
    Role = wf:to_atom(common:q(role, undefined)),
    event({auth, register, Role});
event({auth, register, Role}) -> % {{{2
    Email = common:q(email, undefined),
    Passwd = hash(common:q(password, "")),
    wf:info("Login: ~p, Pass:~p", [Email, Passwd]),
    case db:register(Email, Passwd, Role) of
        #cms_user{email=Email,
                  password=Passwd,
                  role=Role} = User ->
            wf:user(User),
            UserRoles = roles(Role),
            lists:foreach(fun(R) ->
                                  wf:role(R, true)
                          end,
                          UserRoles),
            wf:redirect_from_login("/");
        {error, Any} -> 
            wf:flash(wf:f("Error occured: ~p", [Any])),
            wf:info("Error occured: ~p", [Any]);
        Any -> 
            wf:flash(wf:f("Unhandled error occured: ~p<br>Please contact support to inform about it.", [Any])),
            wf:warning("Error occured: ~p", [Any])
    end;
event({auth, login}) -> % {{{2
    Email = q(email, undefined),
    Passwd = hash(q(password, "")),
    wf:info("Login: ~p, Pass:~p", [Email, Passwd]),
    case db:login(Email, Passwd) of
        [] ->
            wf:flash("Wrong username or password!"),
            ok;
        [#cms_user{email=Email,
                   password=Passwd,
                   role=Role} = User] ->
            UserRoles = roles(Role),
            lists:foreach(fun(R) ->
                                  wf:role(R, true)
                          end,
                          UserRoles),
            wf:user(User),
            wf:info("User: ~p", [User]),
            wf:redirect_from_login("/")
    end;

event({auth, logout}) -> % {{{2
    wf:logout(),
    wf:redirect("/");

event(E) -> % {{{2
    wf:warning("Event ~p occured in module ~p", [E, ?MODULE]).

%% Helper functions {{{1
user() -> % {{{2
    case wf:user() of
        undefined -> #cms_user{};
        U -> U
    end.

roles(Role) -> % {{{2
    lists:dropwhile(fun(R) -> R /= Role end,
                    [R || #{role := R} <- lists:sort(
                                            fun(#{sort := S1},
                                                #{sort := S2}) -> S1 < S2 end, 
                                            db:get_roles())]).

hash(Data) -> % {{{2
    crypto:hash(sha256, Data).
q(Id, Default) -> % {{{2
    case wf:q(Id) of
        "" ->
            Default;
        undefined ->
            Default;
        A -> string:strip(A)
    end.
