%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (emailform).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {email_field, "Email field"},
     {phone_field, "Phone field"},
     {body_field, "Body text area"},
     {submit, "Send button"}
     ].

format_block(F, A) -> % {{{2
    wf:f("bootstrap:~s(~p)", [F, A]).


form_data(submit, A) -> % {{{2
    [_, Block, ToEmail, Classes] = admin:maybe_empty(A, 4),
    {[
      {"Emain to send form", {to_email, ToEmail}}
     ],
     [],
     Block,
     Classes};
form_data(F, [_, Block, Classes]) -> % {{{2
    {[], [], Block, Classes};
form_data(F, []) -> % {{{2
    {[], []}.

save_block(#cms_mfa{id={PID, _}, mfa={M, Fun, A}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, A}}.

%% Block renderers {{{1
email_field(Page, Block, Classes) -> % {{{2
     #panel{
        class="form-group",
        body=#txtbx{
                id=email,
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}}.

phone_field(Page, Block, Classes) -> % {{{2
     #panel{
        class="form-group",
        body=#txtbx{
                id=phone,
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}}.

body_field(Page, Block, Classes) -> % {{{2
     #panel{
        class="form-group",
        body=#txtarea{
                id=text,
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}}.

submit(Page, Block, ToEmail, Classes) -> % {{{2
     #btn{
        type=success,
        size=lg,
        class=["btn-block"|Classes],
        body=common:parallel_block(Page, Block),
        postback={submit, Page, Block, ToEmail},
        delegate=?MODULE
       }.

%% Event handlers % {{{1
event({submit, Page, Block, ToEmail}) -> % {{{2
    Email = common:q(email, "nomail@site.com"),
    Phone = common:q(phone, ""),
    Text = wf:f("Phone: ~s~n~n~s", [Phone, common:q(text, "")]),
    Header = common:parallel_block(Page, common:sub_block(Block, "popup-header")),
    Body = common:parallel_block(Page, common:sub_block(Block, "popup")),

    smtp:send_html(Email, ToEmail, "Form sent from site", Text),
    coldstrap:modal(Header, Body, undefined, [{has_x_button, true}]);
event(Ev) -> % {{{2
    wf:info("~p event ~p", [?MODULE, Ev]).

%% Helpers {{{1

%% Dropdown formatters {{{1