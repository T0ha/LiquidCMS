%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (account).
-compile([export_all, {parse_transform, lager_transform}]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

%% CMS Module interface {{{1
description() -> % {{{2
    "Account related elements".

functions() -> % {{{2
    [
     {email_field, "Login /Email Field"},
     {password_field, "Password Field"},
     {retype_password_field, "Retype Password Field"},
     {login_button, "Login Button"},
     {register_button, "Register Button"}
     ].

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
	
email_field(_Page) -> % {{{2
     #panel{
        class="form-group",
        body=#txtbx{
                id=email,
                placeholder="E-mail"}}.

password_field(_Page) -> % {{{2
    #panel{
       class="form-group",
       body=#pass{
               id=password,
               placeholder="Password"}}.

retype_password_field(_Page) -> % {{{2
     #panel{
        class="form-group",
        body=#pass{
                id=repassword,
                actions=#validate{
                           validators=#confirm_password{
                                         text="Password and confirmation are different",
                                         password=password
                                        }
                          },
                placeholder="Confirm Password"}}.

login_button(_Page) -> % {{{2
     #btn{
        type=success,
        size=lg,
        class="btn-block",
        text="Login",
        postback={auth, login},
        delegate=?MODULE
       }.

register_button(_Page, Role) -> % {{{2
    #btn{
       type=success,
       size=lg,
       class="btn-block",
       text="Register",
       postback={auth, register, Role},
       delegate=?MODULE
      }.

login_form(Page) -> % {{{2
    [
     email_field(Page),
     password_field(Page),
     login_button(Page)
    ].

register_form(Page, Role) -> % {{{2
    [
     email_field(Page),
     password_field(Page),
     retype_password_field(Page),
     register_button(Page, Role)
    ].
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
    % Log In page
    admin:add_page("login", "templates/login.html", undefined, account),
    admin:add_to_block("login", "css", {asset, ["css", "sb-admin-2"]}, 3),

    % Index Setup page
    admin:add_page("index", "templates/setup.html", undefined, index),
    % Admin setup
    admin:add_to_block("index", "admin-setup", {bootstrap, col, ["col-admin", "5", "", ""]}, 1),
    admin:add_to_block("index", "col-admin", {bootstrap, panel, ["admin-panel-header", "admin-panel-body", "", "", ["panel-default"]]}, 1),
    admin:add_to_block("index", "admin-panel-header", {text, ["Admin Account Settings"]}, 1),
    admin:add_to_block("index", "admin-panel-body", {account, email_field, []}, 1),
    admin:add_to_block("index", "admin-panel-body", {account, password_field, []}, 2),
    admin:add_to_block("index", "admin-panel-body", {account, retype_password_field, []}, 3),
    admin:add_to_block("index", "admin-panel-body", {account, register_button, [admin]}, 4),

    % Page setup
    %admin:add_to_block("index", "page-setup", {bootstrap, col, ["col-page", "6", "1", ""]}, 2),
    %admin:add_to_block("index", "col-page", {bootstrap, panel, ["page-panel-header", "admin-panel-body", "", "", ["panel-default"]]}, 1),
    %admin:add_to_block("index", "page-panel-header", {text, ["Admin Account Settings"]}, 1),
    %admin:add_to_block("index", "page-panel-body", {admin, email_field, []}, 1),
    %admin:add_to_block("index", "page-panel-body", {admin, password_field, []}, 2),
    %admin:add_to_block("index", "page-panel-body", {admin, retype_password_field, []}, 3),

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
            wf:redirect_from_login("/admin?page=admin");
        Any -> 
            wf:flash("Error occured: ~p", [Any]),
            wf:warning("Error occured: ~p", [Any])
    end;
event({auth, login}) -> % {{{2
    Email = q(email, undefined),
    Passwd = hash(q(password, "")),
    wf:info("Login: ~p, Pass:~p", [Email, Passwd]),
    case db:login(Email, Passwd) of
        [] ->
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

event(E) -> % {{{2
    wf:warning("Event ~p occured in module ~p", [E, ?MODULE]).

%% Helper functions {{{1
roles(Role) -> % {{{2
    lists:dropwhile(fun(R) -> R == Role end,
                    [
                     root,
                     admin,
                     editor
                    ]).

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
