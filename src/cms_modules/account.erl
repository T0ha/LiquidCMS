%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (account).
-compile([export_all, {parse_transform, lager_transform}]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

?DESCRIPTION(Account).
%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {email_field, "Login/Email Field"},
     {password_field, "Password Field"},
     {retype_password_field, "Retype Password Field"},
     {apply_agreement_cb, "Apply agreement checkbox"},
     {login_button, "Login Button"},
     {logout_button, "Logout Button"},
     {register_button, "Register Button"},
     {change_password_button, "Change password button"},
     {confirm, "Confirm registration handler"},
     {maybe_redirect_to_login, "Redirect to login page if role is not accepted"},
     {restore_password_button, "Restore password button (send email)"}
     ].

format_block(F, A) -> % {{{2
    {wf:f("~p:~s(~p)", [?MODULE, F, A]), undefined}.

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
    };

form_data(maybe_redirect_to_login, A) -> % {{{2
    [_, URL] = admin:maybe_empty(A, 2),

    [ 
     {"URL", {url, URL}}
    ].

save_block(#cms_mfa{mfa={?MODULE, F, [Block, Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, F, [Block, Classes]}};
save_block(#cms_mfa{mfa={?MODULE, register_button, [Block, Role, Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, register_button, [Block, Role, Classes]}};
save_block(#cms_mfa{id={_, _}, mfa={?MODULE, maybe_redirect_to_login, [_Block, URL, _Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{id={"*", "router"}, mfa={?MODULE, maybe_redirect_to_login, [URL]}}.

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
    email_field(Page, "email-field", "").

email_field(Page, Block, Classes) -> % {{{2
      wf:defer(register_button, 
            email,
            #validate{
               validators=#is_email{
                             text=common:parallel_block(Page,
                                                        common:sub_block(Block, "validate")) %
                            }
              }),
     #panel{
        class="form-group",
        body=#txtbx{
                id=email,
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}}.

password_field(Page) -> % {{{2
    password_field(Page, "password-field", "").

password_field(Page, Block, Classes) -> % {{{2
    #panel{
       class="form-group",
       body=#pass{
               id=password,
               class=Classes,
               placeholder=common:parallel_block(Page, Block)}}.

retype_password_field(Page) -> % {{{2
    retype_password_field(Page, "confirm-password-field", "").

retype_password_field(Page, Block, Classes) -> % {{{2
    wf:defer(register_button, 
            repassword,
            #validate{
               validators=#confirm_password{
                             text=common:parallel_block(Page,
                                                        common:sub_block(Block, "validate")), 
                             password=password
                            }
              }),
    wf:defer(change_password_button, 
            repassword,
            #validate{
               validators=#confirm_password{
                             text=common:parallel_block(Page,
                                                        common:sub_block(Block, "validate")), 
                             password=password
                            }
              }),
     #panel{
        class="form-group",
        body=#pass{
                id=repassword,
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}}.

apply_agreement_cb(Page, Block, _Classes) -> % {{{2
    wf:defer(".account-btn", #disable{}),
     #panel{
        class="form-group",
        body=#checkbox{
                text=common:parallel_block(Page, Block),
                id=apply_agreement,
                postback={?MODULE, agree}, 
                delegate=?MODULE
               }}.

login_button(Page) -> % {{{2
    login_button(Page, "login-button", "").

register_button(Page, Role) -> % {{{2
    register_button(Page, "register-button", Role, "").

login_button(Page, Block, Classes) -> % {{{2
     #btn{
        id=login_button,
        type=success,
        size=lg,
        class=["account-btn", "btn-block" | Classes],
        text=common:parallel_block(Page, Block),
        postback={auth, login},
        delegate=?MODULE
       }.

logout_button(Page, Block, Classes) -> % {{{2
    logout_button(Page, Block, link, "", Classes).

logout_button(Page, Block, Type, Size, Classes) -> % {{{2
     #btn{
        id=logout_button,
        type=Type,
        size=Size,
        class=[Classes],
        text=common:parallel_block(Page, Block),
        postback={auth, logout},
        delegate=?MODULE
       }.

register_button(Page, Block, Role, Classes) -> % {{{2
    #btn{
       id=register_button,
       type=success,
       size=lg,
       class=["account-btn", "btn-block" | Classes],
       text=common:parallel_block(Page, Block), % "Register",
       postback={auth, register, wf:to_atom(Role), true},
       delegate=?MODULE
      }.

change_password_button(Page) -> % {{{2
    change_password_button(Page, "change_password_button", "").
change_password_button(Page, Block, Classes) -> % {{{2
    #btn{
       id=change_password_button,
       type=success,
       size=lg,
       class=["account-btn", "btn-block" | Classes],
       text=common:parallel_block(Page, Block), 
       postback={auth, change_password},
       delegate=?MODULE
      }.

restore_password_button(Page) -> % {{{2
    restore_password_button(Page, "restore_password_button", "").
restore_password_button(Page, Block, Classes) -> % {{{2
    #btn{
       id=restore_password_button,
       % size=lg,
       class=[Classes],
       text=common:parallel_block(Page, Block), 
       postback={auth, call_restore_password},
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

confirm(_Page, _Block, _Classes) -> % {{{2
    Data = common:q(confirm, ""),
    case db:confirm(wf:depickle(Data)) of
        {error, Reason} ->
            #panel{class=["alert", "alert-critical"],
                   text=Reason};
        {ok, User} ->
            wf:user(User),
            set_user_roles(User),
            wf:redirect("/")
    end.

change_password() -> % {{{2
    Data = common:q(hex, ""),
    NewPassw = common:q(password, ""),
    if Data /= "" ->
        case db:confirm_change_password(wf:depickle(Data),
                                        hash(unicode:characters_to_binary(NewPassw))) of
            {error, Reason} ->
                #panel{class=["alert", "alert-critical"],
                       text=Reason};
            {ok, User} ->
                wf:user(User),
                wf:flash("<div class='alert alert-error'>Password was successfully changed!</div>"),
                wf:redirect("/")
        end;
    true ->
      wf:flash("<div class='alert alert-error'>URL is not actual now.</div>")
    end.

maybe_redirect_to_login(Page) -> % {{{2
    maybe_redirect_to_login(Page, "/account").

maybe_redirect_to_login(#cms_page{accepted_role=undefined} = Page, URL) -> % {{{2
    maybe_redirect_to_login(Page#cms_page{accepted_role=nobody}, URL);
maybe_redirect_to_login(#cms_page{accepted_role=nobody} = Page, _URL) -> % {{{2
    ?LOG("Not redirect to login: ~p", [Page]),
    Page;
maybe_redirect_to_login(#cms_page{accepted_role=Role} = Page, URL) -> % {{{2
    ?LOG("role: ~p,wf:role(Role):~p, url:~p", [Role,wf:role(Role),URL]),
    case wf:role(Role) of 
        true ->
            ?LOG("Role: ~p", [Role]),
            Page;
        false ->
            ?LOG("Redirect to login: ~p", [Page]),
            wf:redirect_to_login(URL)
    end.

%% Module install routines {{{1
default_data() -> % {{{2
    #{
    cms_role => [
                #cms_role{role = nobody, sort=?ROLE_NOBODY_SORT, name="Nobody"},
                #cms_role{role = admin, sort=?ROLE_ADMIN_SORT, name="Admin"},
                #cms_role{role = root, sort=?ROLE_ROOT_SORT, name="Root"},
                #cms_role{role = editor, sort=?ROLE_EDITOR_SORT, name="Editor"}
              ],
    cms_page => [
                #cms_page{id="confirm", description=[], module=index, accepted_role=nobody, title="LiquidCMS"},
                #cms_page{id="restore", description=[], module=index, accepted_role=nobody, title="LiquidCMS"}
              ],
    cms_mfa => [
                #cms_mfa{id={"index", "body"},
                         mfa={router, common_redirect, [[], "/?page=register"]},
                         sort=1},
                #cms_mfa{id={"confirm", "body"},
                         mfa={account, confirm, [[],[[]]]},
                         sort=1},
                #cms_mfa{id={"login", "css"},
                         mfa={common, asset,[["css","sb-admin-2"]]},
                         sort=3}
                #cms_mfa{id={"restore", "body"},
                         mfa={common, block,["container",["container panel-body"]]},
                         sort=1},
                #cms_mfa{id={"restore", "container"},
                         mfa={account, password_field,["password",[]]},
                         sort=1},
                #cms_mfa{id={"restore", "container"},
                         mfa={account, retype_password_field,["retype_password",[]]},
                         sort=2},
                #cms_mfa{id={"restore", "container"},
                         mfa={account, change_password_button,["chb",[[]]]},
                         sort=3},
                #cms_mfa{id={"restore", "chb"},
                         mfa={common, text,["Change password"]},
                         sort=1},
                #cms_mfa{id={"restore", "password"},
                         mfa={common, text,["New password"]},
                         sort=1},
                #cms_mfa{id={"restore", "retype_password"},
                         mfa={common, text,["Retype new password"]},
                         sort=1},
                #cms_mfa{id={"restore", "retype_password/validate"},
                         mfa={common, text,["Passwords do not match!"]},
                         sort=1}
               ]}.

install() -> % {{{2
    lager:info("Installing ~p module", [?MODULE]),
    % Log In page
    admin:add_page("login", "templates/login.html", undefined, account),
    admin:add_to_block("login", "css", {asset, ["css", "sb-admin-2"]}, 3),

    % Index Setup page
    admin:add_page("index", "templates/setup.html", undefined, index),
    admin:add_to_block("index", "router", {router, page, ["register"]}, 1),

    % Login page
    admin:add_to_block("login", "login-button", {common, text, ["Log In"]}, 5),
    admin:add_to_block("login", "email-field", {common, text, ["Email"]}, 5),
    admin:add_to_block("login", "password-field", {common, text, ["Password"]}, 6),


    % Register page
    admin:add_page("register", "templates/setup.html", undefined, index),

    admin:add_to_block("register", "admin-setup", {bootstrap, col, ["col-admin", "4", "4", ""]}, 1),
    admin:add_to_block("register", "col-admin", {bootstrap, panel, ["admin-panel-header", "admin-panel-body", "", "", ["panel-default"]]}, 1),
    admin:add_to_block("register", "admin-panel-header", {text, ["Admin Account Settings"]}, 1),
    admin:add_to_block("register", "admin-panel-body", {account, email_field, ["email_plh",[[]]]}, 1),
    admin:add_to_block("register", "admin-panel-body", {account, password_field, ["passwd_plh",[[]]]}, 2),
    admin:add_to_block("register", "admin-panel-body", {account, retype_password_field, ["re_passwd_plh",[[]]]}, 3),
    admin:add_to_block("register", "admin-panel-body", {account, register_button, ["reg_btn_text","admin",[[]]]}, 4),
    admin:add_to_block("register", "email_plh", {common, text, ["Email"]}, 5),
    admin:add_to_block("register", "passwd_plh", {common, text, ["Password"]}, 6),
    admin:add_to_block("register", "re_passwd_plh", {common, text, ["Confirm password"]}, 7),
    admin:add_to_block("register", "reg_btn_text", {common, text, ["Register"]}, 8),
    admin:add_to_block("register", "email_plh/validate", {common, text, ["Please provide a valid email address"]}, 5),
    admin:add_to_block("register", "re_passwd_plh/validate", {common, text, ["Password and confirmation are different"]}, 7),

    ok.

%% Event handlers {{{1
event({?MODULE, agree}) -> % {{{2
    case common:q(apply_agreement, "off") of
        "on" ->
            wf:enable(".account-btn");
        _ ->
            wf:disable(".account-btn")
    end;
event({auth, register}) -> % {{{2
    Role = wf:to_atom(common:q(role, undefined)),
    event({auth, register, Role, false});
event({auth, register, Role, DoConfirm}) -> % {{{2
    Email = common:q(email, undefined),
    Passwd = hash(common:q(password, "")),
    case db:register(Email, Passwd, Role, DoConfirm) of
        #cms_user{email=Email,
                  password=Passwd,
                  confirm=Confirm,
                  role=Role} ->
            wf:flash(wf:f("<p class='text-success'>Confirmation letter was sent to ~s.  Please, follow instructions from the letter.</p>", [Email])),
            send_confirmation_email(Email, Confirm);
        {error, Any} -> 
            wf:flash(wf:f("<div class='alert alert-success'>Error occured: ~p</div>", [Any])),
            ?LOG("Error occured: ~p", [Any]);
        Any -> 
            wf:flash(wf:f("Unhandled error occured: ~p<br>Please contact support to inform about it.", [Any])),
            wf:warning("Error occured: ~p", [Any])
    end;
event({auth, login}) -> % {{{2
    Email = q(email, undefined),
    Passwd = hash(q(password, "")),
    case db:login(Email, Passwd) of
        [] ->
            wf:flash("Wrong username or password!"),
            ok;
        [#cms_user{confirm=C}] when C /= 0 ->
            wf:flash("User email is not confirmed. Please, confirm it before login!"),
            ok;
        [User] ->
            set_user_roles(User),
            wf:user(User),
            if User#cms_user.role == admin ->
                wf:redirect_from_login("/admin?page=admin");
            true ->
                wf:redirect_from_login("/")
            end
    end;
event({auth, call_restore_password}) -> % {{{2
  call_restore_password();
event({auth, logout}) -> % {{{2
    wf:logout(),
    % wf:clear_session(),
    wf:redirect("/");
event({auth, forget_password_open_modal, Param}) -> % {{{2
    admin:new_modal("Password restore form",
              {auth, call_restore_password},
              [
              #label{
                text="Input your email for a password recovery",
                class="container"
              },
              #panel{
                class="form-group panel-body",
                body=#txtbx{
                        id=restore_email,
                        class="",
                        placeholder=Param
                        }}
              ]
    )
   ;
event({auth,change_password}) -> % {{{2
    change_password();
event(close_modal) ->
  coldstrap:close_modal();    
event(E) -> % {{{2
    wf:warning("Event ~p occured in module ~p", [E, ?MODULE]).

%% Helper functions {{{1
user() -> % {{{2
    case wf:user() of
        undefined -> #cms_user{};
        U -> U
    end.

set_user_roles(#cms_user{role=Role}) -> % {{{2
    UserRoles = roles(Role),
    lists:foreach(fun(R) ->
                          wf:role(R, true)
                  end,
                  UserRoles).

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

call_restore_password() -> % {{{2
    Email = common:q(email, undefined),
    coldstrap:close_modal(),
    case db:get_user(Email) of
        [#cms_user{email=Email,
                  password=Passwd 
                  }] ->
            wf:flash(wf:f("<p class='text-success'>The password recovery letter was sent to ~s.  Please, follow instructions from the letter.</p>", [Email])),
            send_restore_password_email(Email, Passwd);
        {error, Any} -> 
            wf:flash(wf:f("<div class='alert alert-success'>Error occured: ~p</div>", [Any])),
            ?LOG("Error occured: ~p", [Any]);
        Any -> 
            wf:flash("User with this email address was not found!"),
            wf:warning("User with this email did not found! ~p", [Any])
    end.

send_confirmation_email(_Email, 0) -> % {{{2
    ok;
send_confirmation_email(Email, Confirm) -> % {{{2
    Host = application:get_env(nitrogen, host, "site.com"),
    FromEmail = application:get_env(nitrogen, confirmation_email, wf:f("confirm@~s", [Host])),
    {ok, Text} = wf_render_elements:render_elements(#template{file="templates/mail/confirm.txt", bindings=[{'Confirm', wf:pickle({Email, Confirm})}, {'Host', Host}]}),
    ?LOG("Text: ~p", [Text]),
    smtp:send_html(FromEmail, Email, ["Please, confirm registration on ", Host], Text).

send_restore_password_email(Email, Passwd) -> % {{{2
    Host = application:get_env(nitrogen, host, "nitrogen-site.com"),
    FromEmail = application:get_env(nitrogen, confirmation_email, wf:f("restore@~s", [Host])),
    {ok, Text} = wf_render_elements:render_elements(#template{file="templates/mail/restore.txt", bindings=[{'Confirm', wf:pickle({Email, Passwd})}, {'Host', Host}]}),
    smtp:send_html(FromEmail, Email, ["Please, confirm restore password on ", Host], Text),
    ?LOG("send_restore_password_email: ~p", [Email]).
