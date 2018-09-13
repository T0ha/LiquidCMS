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
form_data(email_field, A) -> % {{{2
    [_PID, Block, Required, Classes] = admin:maybe_empty(A, 4),
    {[],
     [
      #checkbox{
         text="Required",
         % value=true,
         label_position=before,
         id=email_required,
         checked=Required
        }
     ],
     Block,
     Classes
    };
form_data(submit, [Page, Block, ToEmail, Classes]) -> % {{{2
    form_data(submit, [Page, Block, ToEmail, off, Classes]);

form_data(submit, A) -> % {{{2
    [_, Block, ToEmail, Smtp, Classes] = admin:maybe_empty(A, 5),
    {[
      {"Email to send form", {to_email, ToEmail}}
     ],
     [#checkbox{
         text="Use Smtp",
         label_position=before,
         id=use_smtp,
         checked=Smtp
        }],
     Block,
     Classes};
form_data(_F, [_, Block, Classes]) -> % {{{2
    {[], [], Block, Classes};
form_data(_F, []) -> % {{{2
    {[], []}.

save_block(#cms_mfa{id={_PID, _}, mfa={_M, rating=Fun, A}}=Rec) -> % {{{2
    [Block, Min, Max, Step, Size, Classes, _DataAttr] = admin:maybe_empty(A, 7),
    ShowCaption = wf:to_atom(common:q(show_caption, false)),
    ShowClear = wf:to_atom(common:q(show_clear, false)),
    Rec#cms_mfa{mfa={?MODULE, Fun, [Block, Min, Max, Step, Size, ShowCaption, ShowClear, Classes]}};

save_block(#cms_mfa{id={_PID, _}, mfa={_M, submit=Fun,  [B, Email, Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [B, Email, off, Classes]}};
save_block(#cms_mfa{id={_PID, _}, mfa={_M, submit=Fun,  [B, Email, Smtp, Classes, _DataAttr]}}=Rec) -> % {{{2
    Smtp = wf:to_atom(common:q(use_smtp, false)),
    Rec#cms_mfa{mfa={?MODULE, Fun, [B, Email, Smtp, Classes]}};
save_block(#cms_mfa{id={_PID, _}, mfa={_M, email_field=Fun,  [B, Classes, _DataAttr]}}=Rec) -> % {{{2
    Required = wf:to_atom(common:q(email_required, false)),
    Rec#cms_mfa{mfa={?MODULE, Fun, [B, Required, Classes]}};
save_block(#cms_mfa{id={_PID, _}, mfa={_M, Fun,  [B, Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, Fun, [B, Classes]}}.


%% Block renderers {{{1
email_field(Page, Block, Classes) -> % {{{2
    email_field(Page, Block, on, Classes).
email_field(Page, Block, Required, Classes) -> % {{{2

    case Required of
        on ->  
            ValidatorBlock = common:sub_block(Block, "validate"),
            wf:defer(submit,
                     email,
                     #validate{
                        validators=[
                                    #is_email{text=common:parallel_block(Page, ValidatorBlock)}
                                   ]
                       }
                    );

        _ -> ok
    end,
    #panel{
        class="form-group",
        body=#txtbx{
                id=email,
                html_id=common:block_to_html_id(Block),
                class=Classes,
                placeholder=common:parallel_block(Page, Block)}
    }.

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
    submit(Page, Block, ToEmail, off, Classes).

submit(Page, Block, ToEmail, Smtp, Classes) -> % {{{2
    #btn{
        %type=success,
        id=submit,
        size=lg,
        html_id=common:block_to_html_id(Block),
        class=["btn-block"|Classes],
        body=common:parallel_block(Page, Block),
        postback={submit, Page, Block, ToEmail, Smtp},
        delegate=?MODULE
       }.

%% Event handlers % {{{1
event({submit, #cms_page{id=PID}=Page, Block, ToEmail, Smtp}) -> % {{{2
    Phone = wf:to_list(common:q(phone, "")),
    RatingsPL = wf:q_pl(get_form_rating_ids(Page, Block)),
    % ?LOG("Ratings: ~p~n", [RatingsPL]),
    TextForm = unicode:characters_to_binary(wf:to_list(common:q(text, ""))),
    Host = application:get_env(nitrogen, host, "site.com"),
    FromEmailForm = application:get_env(nitrogen, form_email, "form@" ++ Host),
    Subject = "Form sent from " ++ Host,
    Email = wf:to_list(common:q(email, FromEmailForm)),
    Text = [
            add_if_not_empty("Email: ", Email),
            add_if_not_empty("Phone: ", Phone),
            add_if_not_empty("Your ratings: ",
                            [wf:f("~s: ~p~n", [K, V]) || {K, V} <- RatingsPL]),
            "\n",
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
        
    case Smtp of
        on ->
            AuthData = application:get_env(esmpt, smtp_auth_data),
            HostSmtp = application:get_env(esmpt, host),
            Port = application:get_env(esmpt, port, 465),
            Options = [{use_ssl, true}, {host, HostSmtp}, {port, Port}],
            Body = lists:concat([Text]),
            gen_smtpc:send(AuthData, ToEmail, Subject, Body, Options);
        _ ->
            smtp:send_html(FromEmailForm, ToEmail, Subject, Text, Email)
    end,
    ?LOG("Send email to ~p from ~p", [ToEmail, Email]),
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
    [Header, Text, "\n"].
%% Dropdown formatters {{{1
