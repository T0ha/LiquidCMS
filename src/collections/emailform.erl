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
    form_data(submit, [Page, Block, ToEmail, false, Classes]);
form_data(submit, [Page, Block, ToEmail, Smtp, Classes])-> % {{{2
    form_data(submit, [Page, Block, ToEmail, Smtp, none, [], Classes]);
form_data(submit, A) -> % {{{2
    [_, Block, ToEmail, Smtp, Apicall, AddFields, Classes] = admin:maybe_empty(A, 7),
    {[
        {"Email to send form", {to_email, ToEmail}},
        #checkbox{
                text="Use Smtp",
                label_position=before,
                id=use_smtp,
                checked=Smtp
        },
        {"API call",#dd{ id=api,
             value=Apicall,
             options=[none | cms_apis()],
             postback=switch_add_field,
             delegate=?MODULE
        }},
        #btn{
            id=add_field_btn,
            size=md,
            class=["btn-upload btn-block " ++ case lists:member(Apicall,cms_apis()) of
                                                 false -> "hidden";  
                                                 true-> "" 
                                               end
            ],
            text="Add field(api)",
            postback=add_field,
            delegate=?MODULE
        },
        #p{id=for_pretty},
        [#p{id=addfields,
            body=[
                
                #txtbx{text=K, placeholder="field name", class="form-fields"},
                #txtbx{text=V, placeholder="field value", class="form-fields"}
            ]} || {K, V} <- AddFields
        ],
        #span{id=for_insert}
     ],
     [ ],
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

save_block(#cms_mfa{id={_PID, _}, mfa={_M, submit=Fun, [B, Email, Apicall, Classes, _DataAttr]}}=Rec) -> % {{{2
    Smtp = wf:to_atom(common:q(use_smtp, false)),
    AddFields =
        case wf:to_atom(Apicall) of
            none ->
                [];
            _ ->
                Proplist = list_to_proplist(wf:qs(addfields)),
                lists:filter(fun(I)-> I /={} end, Proplist)
        end,
    Rec#cms_mfa{mfa={?MODULE, Fun, [B, Email, Smtp, Apicall, AddFields, Classes]}};
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
    submit(Page, Block, ToEmail, false, Classes).
submit(Page, Block, ToEmail, Smtp, Classes) -> % {{{2
    submit(Page, Block, ToEmail, Smtp, none, [], Classes).
submit(Page, Block, ToEmail, Smtp, Apicall, AddFields, Classes) -> % {{{2
    #btn{
        %type=success,
        id=submit,
        size=lg,
        html_id=common:block_to_html_id(Block),
        class=["btn-block"|Classes],
        body=common:parallel_block(Page, Block),
        postback={submit, Page, Block, ToEmail, Smtp, Apicall, AddFields},
        delegate=?MODULE
       }.

%% Event handlers % {{{1
event({submit, #cms_page{id=PID}=Page, Block, ToEmail, Smtp, Apicall, AddFields}) -> % {{{2
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
        
    
    case Apicall of
        none ->
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
            admin:add_form(PID, Phone, TextForm, Email, RatingsPL);
        ApiName ->
            case string:str(Apicall, "unisender") of % unisender api
                Index when Index > 0 ->
                    [#cms_api{apikey=Key}] = db:get_api(ApiName),
                    PreUrl =  wf:f("~s?api_key=~s", [ApiName, Key]), 
                    Args = lists:append([AddFields,[{"fields[email]",Email}]]),
                    FormatFields = [wf:f("&~s=~s", [K, V])  || {K, V} <- Args],
                    FullUrl = string:concat(PreUrl,FormatFields),
                    ?LOG("FullUrl: ~s", [FullUrl]),
                    {ok, {{_Version, 200, ResultPhrase}, _Headers, _Body}} = httpc:request(get, {FullUrl, []}, [], []),
                    ?LOG("Result:~p",[ResultPhrase]);
                _ ->
                    undefined
            end
    end,
    coldstrap:close_modal(),
    wf:flash(FlashID, Flash);
event(switch_add_field) -> % {{{2
    case wf:to_atom(common:q(api, "false")) of
        none ->
            wf:wire(add_field_btn, #add_class { class=hidden, speed=40 }),
            wf:wire(addfields, #add_class { class=hidden, speed=40 });
        _ ->
            wf:wire(add_field_btn, #remove_class { class=hidden, speed=40 }),
            wf:wire(addfields, #remove_class { class=hidden, speed=40 })
    end;
event(add_field) -> % {{{2
    wf:insert_before(for_insert,
          #p{id=addfields,
             class="flex",
             body=[
                 #txtbx{placeholder="field name", class="form-fields"},
                 #txtbx{placeholder="field value", class="form-fields"}
                  ]
            }
    );
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

list_to_proplist([]) -> [];
list_to_proplist([K, V | Tail]) ->
  [if K/="" ->{K, V};
    true -> {} end | list_to_proplist(Tail)].

cms_apis() -> % {{{2
    [Name || #{ name := Name} <- db:get_apis()].