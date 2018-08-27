%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (emailform).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-include("cms.hrl").

?DESCRIPTION(Form to email).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {email_field, "Email field"},
     {phone_field, "Phone field"},
     {body_field, "Body text area"},
     {rating, "Star Ratings"},
     {submit, "Send button"}
     ].

format_block(F, [Block|_]=A) -> % {{{2
    {wf:f("~p:~s(~p)", [?MODULE, F, A]), Block}.

form_data(rating, A) -> % {{{2
    [_, Block, Min, Max, Step, Size, ShowCaption, ShowClear, Classes] = admin:maybe_empty(A, 9),
    {[
      {"Min", {min, Min}},
      {"Max", {max, Max}},
      {"Step", {step, Step}},
      {"Size",
       #dd{
          id=size,
          value=Size,
          options=["xl", "lg", "md", "sm", "xs"]
         }
      },
      #checkbox{
         text="Show caption",
         value=true,
         label_position=before,
         id=show_caption,
         checked=ShowCaption
        },
      #checkbox{
         text="Show Clear",
         value=true,
         label_position=before,
         id=show_clear,
         checked=ShowClear
        }
     ],
     [],
     Block,
     Classes};
form_data(submit, A) -> % {{{2
    [_, Block, ToEmail, Classes] = admin:maybe_empty(A, 4),
    {[
      {"Email to send form", {to_email, ToEmail}}
     ],
     [],
     Block,
     Classes};
form_data(_F, [_, Block, Classes]) -> % {{{2
    {[], [], Block, Classes};
form_data(_F, []) -> % {{{2
    {[], []}.

save_block(#cms_mfa{id={_PID, _}, mfa={_M, rating=Fun, A}}=Rec) -> % {{{2
    % ?LOG("save rating: ~p", [Rec]),
    [Block, Min, Max, Step, Size, Classes, _DataAttr] = admin:maybe_empty(A, 7),
    ShowCaption = wf:to_atom(common:q(show_caption, false)),
    ShowClear = wf:to_atom(common:q(show_clear, false)),
    Rec#cms_mfa{mfa={?MODULE, Fun, [Block, Min, Max, Step, Size, ShowCaption, ShowClear, Classes]}};

save_block(#cms_mfa{id={_PID, _}, mfa={_M, submit=Fun,  [B, Email, Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [B,Email, Classes]}};
save_block(#cms_mfa{id={_PID, _}, mfa={_M, Fun,  [B, Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [B, Classes]}}.


%% Block renderers {{{1
email_field(Page, Block, Classes) -> % {{{2
     #panel{
        class="form-group",
        body=#txtbx{
                id=email,
        html_id=common:block_to_html_id(Block),
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}}.

phone_field(Page, Block, Classes) -> % {{{2
     #panel{
        class="form-group",
        body=#txtbx{
                id=phone,
                html_id=common:block_to_html_id(Block),
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}}.

body_field(Page, Block, Classes) -> % {{{2
     #panel{
        class="form-group",
        body=#txtarea{
                id=text,
                html_id=common:block_to_html_id(Block),
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}}.

rating(_Page, Block, Min, Max, Step, Size, ShowCaption, ShowClear, _Classes) -> % {{{2
    #textbox{id=common:block_to_html_id(Block),
        html_id=common:block_to_html_id(Block),
        class=["rating", "rating-loading"],
        data_fields=[
                     {min, Min},
                     {max, Max},
                     {step, Step},
                     {size, Size},
                     {"show-caption", ShowCaption},
                     {"show-clear", ShowClear}
                    ]}.

submit(Page, Block, ToEmail, Classes) -> % {{{2
     #btn{
        %type=success,
        size=lg,
        html_id=common:block_to_html_id(Block),
        class=["btn-block"|Classes],
        body=common:parallel_block(Page, Block),
        postback={submit, Page, Block, ToEmail},
        delegate=?MODULE
       }.

%% Event handlers % {{{1
event({submit, #cms_page{id=PID}=Page, Block, ToEmail}) -> % {{{2
    Phone = wf:to_list(common:q(phone, "")),
    RatingsPL = wf:q_pl(get_form_rating_ids(Page, Block)),
    ?LOG("Ratings: ~p~n", [RatingsPL]),
    TextForm = unicode:characters_to_binary(wf:to_list(common:q(text, ""))),
    Host = application:get_env(nitrogen, host, "site.com"),
    FromEmail = application:get_env(nitrogen, form_email, "form@" ++ Host),
    Email = wf:to_list(common:q(email, FromEmail)),
    Text = [
            add_if_not_empty("E-mail: ", Email),
            add_if_not_empty("Phone: ", Phone),
            add_if_not_empty("Your ratings: ",
                            [wf:f("~s: ~p~n", [K, V]) || {K, V} <- RatingsPL]),
            TextForm
           ],
    Flash = #span{
               class=["alert-warning"],
               body=common:parallel_block(Page, common:sub_block(Block, "flash-message"))
              },

    FlashID = wf:temp_id(),
    wf:defer(#event{type=timer,
                    delay=15000, 
                    target=FlashID,
                    actions=#hide{effect=blind, speed=40}
                   }),
    smtp:send_html(FromEmail, ToEmail, "Form sent from site", Text),
    admin:add_form(PID, Phone, TextForm, Email, RatingsPL),
    wf:flash(FlashID, Flash);
event(Ev) -> % {{{2
    ?LOG("~p event ~p", [?MODULE, Ev]).

%% Helpers {{{1
get_form_rating_ids(#cms_page{id=PID}, Block) -> % {{{2
    db:transaction(
      fun() ->
              [#cms_mfa{id={PID, FormBlock}} | _] = mnesia:match_object(
                                                      #cms_mfa{id={PID, '_'},
                                                               mfa={emailform, submit, [Block, '_', '_']},
                                                               _='_'}),
              Elements = mnesia:match_object(
                           #cms_mfa{id={PID, FormBlock}, 
                                    mfa={emailform, rating, '_'},
                                    _='_'}),
              [common:block_to_html_id(B) || #cms_mfa{mfa={_, _, [B | _]}} <- Elements]
      end).

add_if_not_empty(_Header, undefined) -> % {{{2
    "";
add_if_not_empty(_Header, "") -> % {{{2
    "";
add_if_not_empty(Header, Text) -> % {{{2
    [Header, "\n", Text].
%% Dropdown formatters {{{1
