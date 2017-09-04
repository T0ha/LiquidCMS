%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (emailform).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

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
form_data(F, [_, Block, Classes]) -> % {{{2
    {[], [], Block, Classes};
form_data(F, []) -> % {{{2
    {[], []}.

save_block(#cms_mfa{id={PID, _}, mfa={M, rating=Fun, A}}=Rec) -> % {{{2
    %?LOG("save rating: ~p", [Rec]),
    [Block, Min, Max, Step, Size, Classes] = admin:maybe_empty(A, 6),

    ShowCaption = wf:to_atom(common:q(show_caption, false)),
    ShowClear = wf:to_atom(common:q(show_clear, false)),
    %?LOG("Show: ~p ~p~n", [ShowCaption, ShowClear]),
    Rec#cms_mfa{mfa={?MODULE, Fun, [Block, Min, Max, Step, Size, ShowCaption, ShowClear, Classes]}};

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

rating(Page, Block, Min, Max, Step, Size, ShowCaption, ShowClear, Classes) -> % {{{2
    #textbox{id=common:block_to_html_id(Block),
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
        class=["btn-block"|Classes],
        body=common:parallel_block(Page, Block),
        postback={submit, Page, Block, ToEmail},
        delegate=?MODULE
       }.

%% Event handlers % {{{1
event({submit, Page, Block, ToEmail}) -> % {{{2
    Email = wf:to_list(common:q(email, "nomail@site.com")),
    Phone = wf:to_list(common:q(phone, "")),
    RatingsPL = wf:q_pl(get_form_rating_ids(Page, Block)),
    ?LOG("Ratings: ~p~n", [RatingsPL]),
    Text = [
            add_if_not_empty("E-mail: ", Email),
            add_if_not_empty("Phone: ", Phone),
            add_if_not_empty("Your ratings: ",
                            [wf:f("~s: ~p~n", [K, V]) || {K, V} <- RatingsPL]),
            wf:to_list(common:q(text, ""))
           ],
    Header = common:parallel_block(Page, common:sub_block(Block, "popup-header")),
    Body = common:parallel_block(Page, common:sub_block(Block, "popup")),

    smtp:send_html(Email, ToEmail, "Form sent from site", Text),
    coldstrap:modal(Header, Body, undefined, [{has_x_button, true}]);
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
