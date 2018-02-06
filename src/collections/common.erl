-module(common).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").
-include("records.hrl").

?DESCRIPTION(Common elements).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     %{parallel_block, "Parallell Group of Blocks"},
     %{waterfall, "Waterfall Block"},
     {asset, "Static Asset"},
     {img, "Image"},
     {template, "Template"},
     {list, "Items List"},
     {list_item, "List Item"},
     {link_url, "Link"},
     {block, "Block (div)"},
     %{icon, "Icon"},
     %{script, "Inline Script"},
     {text, "Text with HTML"}
     %{full_block, "One Column Row"}
     ].


format_block(block, [Block, Classes, DataAttr]) -> % {{{2
    {wf:f("Block (div): ~s(class=~p) (attr:~p)", [Block, Classes, DataAttr]), Block};
format_block(list, [Block, Numbered, Classes, DataAttr]) -> % {{{2
    {wf:f("List: ~s(numbered=~s, class=~p, attr:~p)", [Block, Numbered, Classes, DataAttr]), Block};
format_block(list_item, [Block, Classes, DataAttr]) -> % {{{2
    {wf:f("List Item: ~s(class=~p, attr:~p)", [Block, Classes, DataAttr]), Block};
format_block(link_url, [Block, URL, Classes, DataAttr]) -> % {{{2
    {wf:f("Link: ~s(href=~s, class=~p, attr:~p)", [Block, URL, Classes, DataAttr]), Block};
format_block(text, [Text]) -> % {{{2
    {#panel{body=Text}, undefined};
format_block(template, [TID]) -> % {{{2
    [#cms_template{name=Name}] = db:get_template(TID),
    {wf:f("Template: ~s(~p)", [Name, TID]), undefined};
format_block(img, [AID, Classes]) -> % {{{2
    [#cms_asset{type=image, file=File, name=Name}|_] = db:get_asset(AID),
    {wf:f("Image: ~s(file=~p, classes=~p)", [Name, File, Classes]), undefined};
format_block(asset, [AID]) -> % {{{2
    [#cms_asset{type=Type, file=File, name=Name}|_] = db:get_asset(AID),
    {wf:f("Asset ~s: ~s(~p)", [Type, Name, File]), undefined};
format_block(F, A) -> % {{{2
    % ?LOG("format_block: F:~p, A:~p",[F,A]),
    {wf:f("common:~s(~p)", [F, A]), undefined}.


form_data(list, A) -> % {{{2
    [_, Block, Numbered, Classes, DataAttr] = admin:maybe_empty(A, 5),
    {[
       #checkbox{
          text="Numbered",
          value="true",
          label_position=before,
          id=numbered,
          checked=Numbered
         }
     ],
     [], Block, Classes, DataAttr};
form_data(link_url, A) -> % {{{2
    [_, Block, URL, Classes, DataAttr] = admin:maybe_empty(A, 5),
    {[
      {"URL",
       #txtbx{
          id=url,
          text=URL,
          placeholder="http://yoursite.com"
         }}
     ],
     [], Block, Classes, DataAttr};
form_data(text, A) -> % {{{2
    [_, Text] = admin:maybe_empty(A, 2),
    [
     #wysiwyg{class=["form-control"],
              id=text_mfa,
              html=Text,
              buttons=[
                       #panel{
                          class="btn-group",
                          body=[
                                #wysiwyg_button{body="B",
                                                func="bold",
                                                class=["btn", "btn-default"]},
                                #wysiwyg_button{body="<i>I</i>",
                                                func="italic",
                                                class=["btn", "btn-default"]},
                                #wysiwyg_button{body="<u>U</u>",
                                                func="underline",
                                                class=["btn", "btn-default"]},
                                #wysiwyg_button{body="<s>S</s>",
                                                func="strikethrough",
                                                class=["btn", "btn-default"]}
                               ]},
                       #panel{
                          class="btn-group",
                          body=[
                                #wysiwyg_button{
                                   body="<i class='fa fa-align-left'></i>",
                                   func="justifyleft",
                                   class=["btn", "btn-default"]},
                                #wysiwyg_button{
                                   body="<i class='fa fa-align-center'></i>",
                                   func="justifycenter",
                                   class=["btn", "btn-default"]},
                                #wysiwyg_button{
                                   body="<i class='fa fa-align-right'></i>",
                                   func="justifyright",
                                   class=["btn", "btn-default"]},
                                #wysiwyg_button{
                                   body="<i class='fa fa-align-justify'></i>",
                                   func="justifyfull",
                                   class=["btn", "btn-default"]}
                               ]},
                       #panel{
                          class="btn-group",
                          body=[
                                #wysiwyg_button{
                                   body="<i class='fa fa-undo'></i>",
                                   func="undo",
                                   class=["btn", "btn-default"]},
                                #wysiwyg_button{
                                   body="<i class='fa fa-repeat'></i>",
                                   func="redo",
                                   class=["btn", "btn-default"]},
                                #wysiwyg_button{
                                   body="<i class='fa fa-close'></i>",
                                   func="removeFormat",
                                   class=["btn", "btn-default"]}
                               ]},
                       #panel{
                          class="btn-group",
                          body=[
                                #wysiwyg_button{
                                   body="<i class='fa fa-list-ol'></i>",
                                   func="insertorderedlist",
                                   class=["btn", "btn-default"]},
                                #wysiwyg_button{
                                   body="<i class='fa fa-list-ul'></i>",
                                   func="insertunorderedlist",
                                   class=["btn", "btn-default"]},
                                #wysiwyg_button{
                                   body="<i class='fa fa-outdent'></i>",
                                   func="outdent",
                                   class=["btn", "btn-default"]},
                                #wysiwyg_button{
                                   body="<i class='fa fa-indent'></i>",
                                   func="indent",
                                   class=["btn", "btn-default"]}
                               ]},
                       #panel{
                          class="btn-group",
                          body=[
                                #link{
                                   class=[
                                          "btn",
                                          "btn-default",
                                          "dropdown-toggle"
                                         ],
                                   text="H<i class='caret'></i>",
                                   data_fields=[
                                                {toggle, dropdown}
                                               ]},
                                #list{
                                   class="dropdown-menu",
                                   numbered=flase,
                                   body=[ 
                                         #listitem{
                                            body=
                                            #wysiwyg_button{
                                            body="H1",
                                            func="heading h1",
                                            class=["btn", "btn-default"]}
                                           },
                                         #listitem{
                                            body=
                                         #wysiwyg_button{
                                            body="H2",
                                            func="heading h2",
                                            class=["btn", "btn-default"]}
                                           },
                                         #listitem{
                                            body=
                                         #wysiwyg_button{
                                            body="H3",
                                            func="heading h3",
                                            class=["btn", "btn-default"]}
                                           },
                                         #listitem{
                                            body=
                                         #wysiwyg_button{
                                            body="H4",
                                            func="heading h4",
                                            class=["btn", "btn-default"]}}
                                        ]}
                               ]}
                      ]}
    ];
form_data(template, A) -> % {{{2
    [_, TID] = admin:maybe_empty(A, 2),
    Templates = db:get_templates(),
    DropdDown = [ {Path, Name} || #{file := Path, name := Name} <- Templates],
    [
     #span{text="Template"},
     #dd{
        id=template,
        value=TID,
        options=DropdDown
       }
    ];
form_data(img, A) -> % {{{2
    ?LOG("Image: ~p", [A]),
    [_, AID, Classes] = admin:maybe_empty(A, 3),

    {[
      {"Image",
       assets_dropdown(image, AID)}
    ],
    [], "", Classes};

form_data(asset, A) -> % {{{2
    ?LOG("Asset: ~p", [A]),
    [_, AID] = admin:maybe_empty(A, 2),
    Type = case db:get_asset(AID) of
               [] -> binary;
               [#cms_asset{type=T}|_] -> T
           end,

    {[
      {"Asset Type",
       #dd{
          id=asset_type,
          value=Type,
          postback={asset, type, change},
          delegate=?MODULE,
          options=asset_types()
         }},

      {"Asset",
       assets_dropdown(Type, AID)}
    ],
    []}.

save_block(#cms_mfa{mfa={common, list, [Block, Classes, DataAttr]}}=Rec) -> % {{{2
    Num = wf:to_atom(common:q(numbered, "false")),
    Rec#cms_mfa{mfa={common, list, [Block, Num, Classes, DataAttr]}};
save_block(#cms_mfa{mfa={common, link_url, [Block, URL, Classes, DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={common, link_url, [Block, URL, Classes, DataAttr]}};
save_block(#cms_mfa{mfa={common, text, [_Block, _Classes, _DataAttr]}}=Rec) -> % {{{2
    HTML = q(text_mfa, ""),
    Rec#cms_mfa{mfa={common, text, [HTML]}};
save_block(#cms_mfa{mfa={common, img, [_Block, StringId, Classes, _DataAttr]}}=Rec) -> % {{{2
    Id = string:tokens(StringId, "."),
    Rec#cms_mfa{mfa={common, img, [Id, Classes]}};
save_block(#cms_mfa{mfa={common, asset, [_Block, "image", StringId, Classes, _DataAttr]}}=Rec) -> % {{{2
    Id = string:tokens(StringId, "."),
    Rec#cms_mfa{mfa={common, img, [Id, Classes]}};
save_block(#cms_mfa{mfa={common, asset, [_Block, _Type, StringId, _Classes, _DataAttr]}}=Rec) -> % {{{2
    Id = string:tokens(StringId, "."),
    Rec#cms_mfa{mfa={common, asset, [Id]}};
save_block(#cms_mfa{mfa={common, template, [_Block, File, _Classes, _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={common, template, [File]}}.

%% Block renderers {{{1
parallel_block(#cms_page{id = PID} = Page, Block) -> % {{{2
    % ?LOG("~nparallel_block333: ~p ",[Block]),
    [maybe_render_block(Page, MFA) || MFA <- db:get_mfa(PID, Block)].


waterfall(#cms_page{id = PID} = Page, Block) -> % {{{2
    Functions = db:get_mfa(PID, Block),
    ?LOG("Waterfall: ~p", [Functions]),
    lists:foldl(fun(#cms_mfa{mfa={M, F, Args}}, #cms_page{}=P) ->
                        apply(M, F, [P | Args]);
                   (#cms_mfa{mfa=Fun}, #cms_page{}=P) when is_function(Fun) ->
                                    Fun(P);
                   (_, P) -> P
                end, 
                Page,
                Functions).

asset(_Page, AID) -> % {{{1 % {{{2
    Assets = db:get_asset(AID),
    Debug = application:get_env(nitrogen, debug, false),
    #cms_asset{file=Path,
               type=Type} = case {length(Assets), Debug} of
                                {1, _} -> hd(Assets);
                                {L, Debug} when L > 1 ->
                                    hd([A || A <- Assets,
                                             A#cms_asset.minified == not Debug]);
                                _ -> #cms_asset{type=none}
                            end,
    case Type of
        script ->
            "<script src='" ++ wf:to_list(Path) ++ "' type='text/javascript' charset='utf-8'></script>";
        css ->
            "<link href='" ++ wf:to_list(Path) ++ "' rel='stylesheet'>";
        image ->
            #image{image=wf:to_list(Path)};
        none -> wf:f("No asset: ~p: ~p", [AID, Assets]);
        _ -> []
    end.

img(_Page, AID, Classes) -> % {{{1 % {{{2
    case db:get_asset(AID) of
        [#cms_asset{file=Path,
                    type=image}|_] ->
            #image{class=Classes,
                   image=wf:to_list(Path)};
        none -> wf:f("No image: ~p: ~p", [AID]);
        _ -> []
    end.

template(#cms_page{id=PID}=Page, TID) -> % {{{2
    template(#cms_page{id=PID}=Page, TID, []).

template(#cms_page{id=_PID}=Page, TID, AdditionalBindings) -> % {{{2
    ?LOG("Page for template: ~p, TID: ~p", [Page, TID]),
    [#cms_template{file=File,
                   bindings=Bindings}] = db:get_template(TID),

    #template{file=File,
              bindings=[{'Page', Page} | Bindings ++ AdditionalBindings]}.

list(Page, Block, Classes) -> % {{{2
    list(Page, Block, false, Classes, []).

list(Page, Block, true, Classes) -> % {{{2
    list(Page, Block, true, Classes, []);
list(Page, Block, false, Classes) -> % {{{2
    list(Page, Block, false, Classes, []).

list(Page, Block, Numbered, Classes, DataAttr) -> % {{{2
    Items = parallel_block(Page, Block),
    #list{body=Items,
          numbered=Numbered,
          class=Classes,
          html_id=block_to_html_id(Block),
          data_fields=DataAttr
    }.

list_item(Page, ItemID) -> % {{{2
    list_item(Page, ItemID, []).

list_item(Page, ItemID, Classes) -> % {{{2
    list_item(Page, ItemID, Classes, []).
list_item(Page, ItemID, Classes, DataAttr) -> % {{{2
    #listitem{
       html_id=block_to_html_id(ItemID),
       body=parallel_block(Page, ItemID),
       class=Classes,
       data_fields = DataAttr
      }.

link_url(Page, Block, URL) -> % {{{2
    link_url(Page, Block, URL, []).
link_url(Page, Block, URL, Classes) -> % {{{2
    link_url(Page, Block, URL, Classes, []).
link_url(Page, Block, URL, Classes, DataAttr) -> % {{{2
    #link{
       url=URL, 
       html_id=block_to_html_id(Block),
       class=Classes,
       body=parallel_block(Page, Block),
       data_fields = DataAttr
      }.

link_event(Page, Block, Event) -> % {{{2
    link_event(Page, Block, Event, []).
link_event(Page, Block, Event, Classes) -> % {{{2
    #link{
       html_id=block_to_html_id(Block),
       actions=Event,
       class=Classes,
       body=parallel_block(Page, Block)
      }.

icon(_Page, Font, Name, Classes) -> % {{{2
    icon(Font, Name, Classes).

icon(Font, Name, Classes) -> % {{{2
    Cls = [Font, wf:f("~s-~s", [Font, Name]) | Classes],
    Class = string:join(Cls, " "),
    wf:f("<i class='~s'></i>", [Class]).

script(_Page, Script) -> % {{{2
    wf:wire(#script{script=Script}).

text(_Page, Block, Text) -> % {{{2
    maybe_wrap_to_edit(Text, Block, wf:role(editor)).

block(Page, Block, Classes) -> % {{{2
    block(Page, Block, Classes, []).
block(Page, Block, Classes, DataAttr) -> % {{{2
    #panel{
       html_id=block_to_html_id(Block),
       class=Classes,
       body=common:parallel_block(Page, Block),
       data_fields = DataAttr
      }.

full_block(_Page, Body) -> % {{{2
    #bs_row{
       body=#bs_col{
               cols={lg, 12},
               body=Body}}.

kv_item(_Page, K, PID) -> % {{{2
    {wf:to_list(K), wf:to_list(PID)}.

%% Event handlers % {{{1
event({asset, type, change}) -> % {{{2
    AssetType = wf:to_atom(q(asset_type, "image")),
    wf:replace(asset_id, assets_dropdown(AssetType));

event(Ev) -> % {{{2
    ?LOG("~p event ~p", [?MODULE, Ev]).

%% Helpers {{{1
block_to_html_id(Block) -> % {{{2
    re:replace(Block, "/", "-",[global, {return, list}]).

format_private_block([$+ | Block]) -> % {{{2
    Block;
format_private_block(Block) -> % {{{2
    Block.

is_private_block([$+ | _Block]) -> % {{{2
    true;
is_private_block(_Block) -> % {{{2
    false.
private_block(Block) -> % {{{2
    wf:f("+~s", [Block]).

sub_block(Block, Sub) -> % {{{2
    wf:to_list(Block) ++ "/" ++ wf:to_list(Sub).

q(Id, Default) -> % {{{2
    case wf:q(Id) of
        "" ->
            Default;
        undefined ->
            Default;
        A -> unicode:characters_to_list(string:strip(A))
    end.


module_by_function(FunTuple) -> % {{{2
    {ok, Files} = file:list_dir("ebin"),
    Mods = [wf:to_atom(filename:rootname(F)) || F <- Files, filename:extension(F) /= ".app"],
    [M || M <- Mods,
          M /= cms_collection_default,
          F <- M:module_info(exports),
          F == FunTuple].

maybe_render_block(Page, #cms_mfa{settings=#{filters := Filters}}=MFA) -> % {{{2
    ?LOG("Filters: ~p", [Filters]),
    render_block(apply_filters(Filters), Page, MFA);
maybe_render_block(Page, MFA) -> % {{{2
    render_block(true, Page, MFA).

render_block(false, _, _) -> % {{{2
    ?PRINT("Dont't show"),
    "";
render_block(true, Page, #cms_mfa{id=_Id, mfa={?MODULE, text=F, Args}}=MFA) -> % {{{2
    apply(?MODULE, F, [Page, MFA | Args]);
render_block(true, Page, #cms_mfa{mfa={M, F, Args}}) -> % {{{2
    apply(M, F, [Page | Args]);
render_block(true, Page, #cms_mfa{mfa=Fun}) when is_function(Fun) -> % {{{2
    ?LOG("render_block: ~p fun:~p", [Page,Fun]),
    Fun(Page).

apply_filters(["", "", ""]) -> % {{{2
    true;
apply_filters([undefined, undefined, undefined]) -> % {{{2
    true;
apply_filters(["", "", Role]) -> % {{{2
    #cms_user{role=R} = account:user(),
    ?LOG("Am I ~p - ~p", [Role, Role == R]),
   wf:to_atom(Role) == R;
apply_filters([K, V, ""]) -> % {{{2
    D = wf:q(wf:to_atom(K)),
    ?LOG("K: ~p, V: ~p, Q: ~p", [K, V, D]),
    D == wf:to_list(V);
apply_filters([K, V, R]) -> % {{{2
    #cms_user{role=Role} = account:user(),
    (Role == wf:to_atom(R)) and wf:q(wf:to_atom(K)) == wf:to_list(V);
apply_filters(_) -> % {{{2
    true.

maybe_list(L) when is_list(L) -> % {{{2
    L;
maybe_list(L) -> % {{{2
    [L].
maybe_wrap_to_edit(Text, _Block, false) -> % {{{2
    Text;
maybe_wrap_to_edit(Text, #cms_mfa{id={"admin", _}}, true) -> % {{{2
    Text;
maybe_wrap_to_edit(Text, #cms_mfa{id={_PID, Block}, sort=S}=MFA, true) -> % {{{2
    #panel{
       id=common:block_to_html_id(wf:f("~s-~p", [Block, S])),
       body=Text,
       style="border: #c33 1px dashed; cursor: text;padding: 10px;",
       actions=?POSTBACK({?MODULE, edit, text, MFA, Text}, admin)
      }.


%% Dropdown formatters {{{1
asset_types() -> % {{{2
    [{css, "CSS"},
     {script, "Script"},
     {image, "Image"},
     {binary, "Other"}].

assets_dropdown(AssetType) -> % {{{2
    assets_dropdown(AssetType, "").

assets_dropdown(AssetType, AID) -> % {{{2
    AssetsDup = db:get_assets(AssetType),
    Assets = sets:to_list(sets:from_list(AssetsDup)),
    Options = [ #option{value=string:join(Id, "."),
                        text=Name} || #{id := Id,
                                        name := Name} <- Assets],
     #dd{
        id=asset_id,
        value=string:join(AID, "."),
        options=Options
       }.
