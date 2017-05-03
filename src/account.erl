%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (account).
-compile([export_all, {parse_transform, lager_transform}]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

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
	
login_form(Page) -> % {{{2
    [
     #panel{
        class="form-group",
        body=#txtbx{
                id=email,
                placeholder="E-mail"}},
     #panel{
        class="form-group",
        body=#pass{
                id=password,
                placeholder="Passwprd"}},
     #btn{
        type=success,
        size=lg,
        class="btn-block",
        text="Login",
        postback={auth, login},
        delegate=?MODULE
       }
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
    admin:add_page("login", "templates/login.html", undefined, account),
    admin:add_to_block("login", "css", {asset, ["css", "sb-admin-2"]}, 3),
    ok.

%% Event handlers {{{1
event({auth, login}) -> % {{{2
    Email = common:q(email, undefined),
    Passwd = common:q(password, undefined),
    wf:info("Login: ~p, Pass:~p", [Email, Passwd]),
    case db:login(Email, hash(Passwd)) of
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
