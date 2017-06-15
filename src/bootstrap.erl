-module(bootstrap).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").
-include("records.hrl").

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {navbar, "Navigation Bar"},
     {nav_item, "Navigation Bar button"},
     {panel, "Panel"},
     %{slider, "Slider"},
     %{dropdown, "Dropdown"},
     %{script, "Inline Script"},
     {tabs, "Tabs"},
     {tab, "Tab"},
     {row, "Row"},
     {col, "Column"}
     %{full_block, "One Column Row"}
     ].

format_block(col, [Block, Width, Offset, Classes]) -> % {{{2
    {wf:f("Column: ~s(width=~p, offset=~p, classes=~p)",
         [Block, Width, Offset, Classes]),
    Block};
format_block(panel, [HeaderBlock, BodyBlock, AddonsBlock, FooterBlock, Classes]) -> % {{{2
    {wf:f("Panel(header_block=~p, body_block=~p, addons_block=~p, footer_block=~p, classes=~p)",
         [HeaderBlock, BodyBlock, AddonsBlock, FooterBlock, Classes]),
     BodyBlock};
format_block(full_block, [Block, RowClass, ColClass]) -> % {{{2
    {wf:f("One Column Row: ~s(row_class=~p, col_class=~p)",
         [Block, RowClass, ColClass]),
    Block};
format_block(navbar, [Block, Classes]) -> % {{{2
    [[$+|B], _] = string:tokens(Block, "/"),
    {wf:f("NavBar: ~s(class=~p)", [B, Classes]), B};
format_block(nav_item, [Block]) -> % {{{2
    [B, _] = string:tokens(Block, "/"),
    {wf:f("NavBarButton: ~s", [B]), B};
format_block(F, [Block|_]=A) -> % {{{2
    {wf:f("bootstrap:~s(~p)", [F, A]), Block}.

form_data(col, A) -> % {{{2
    [_, Block, W, O, Classes] = admin:maybe_empty(A, 5),

    {[], 
     [
      {"Width (1..12)", {width, W}},
      {"Offset", {offset, O}}
     ],
     Block,
     Classes
    };
form_data(panel, A) -> % {{{2
    [_, HeaderBlock, Block, AddonsBlock, FooterBlock, C] = admin:maybe_empty(A, 6),
    [Classes, Context] = admin:maybe_empty(C, 2),
    {[
      {"Block for panel header",
      #txtbx{
         id=panel_header_block,
         text=HeaderBlock,
         placeholder="Block name for panel header (leave blank for none)"
        }},
      {"Block for panel addons",
      #txtbx{
         id=panel_addons_block,
         text=AddonsBlock,
         placeholder="Block name for panel addons (leave blank for none)"
        }},
      {"Block for panel footer",
      #txtbx{
         id=panel_footer_block,
         text=FooterBlock,
         placeholder="Block name for panel footer (leave blank for none)"
        }}
     ],
     [
      {"Context",
       #dd{
          id=context,
          value=Context,
          options=context_classes(panel)
         }}
     ],
     Block,
     Classes
    };
form_data(slider, A) -> % {{{2
    {[],
     [
      {"Position",
       #dd{
          id=position,
          options=position_classes(navbar)
         }
      },
      {"Alignment",
       #dd{
          id=alignment,
          options=alignment_classes(navbar)
         }
      },
      #bs_col{
         cols={lg, 3},
         body=[
               %#span{text="Color"},
               #checkbox{
                  text="Inverse",
                  value="inverse",
                  label_position=before,
                  id=inverse,
                  checked=false
                 }
              ]}
     ]
    };
form_data(nav_item, A) -> % {{{2
    [PID, NavBlock, Classes] = admin:maybe_empty(A, 3),
    {Block, Text, URL} = get_navitem_data(PID, NavBlock),
    {[
      #panel{
         id=submenu_box,
         body=
         #checkbox{
            text="Dropdown",
            id=submenu,
            postback=submenu,
            delegate=?MODULE,
            checked=(URL == "")
           }
        },
      #panel{
         id=url_box,
         show_if=(URL /= ""),
         body=[
               #span{text="URL"},
               #txtbx{
                  id=url,
                  text=URL,
                  placeholder="https://yourdomain.com"
                 }
              ]},
      {"Text",
       {text, Text}}
     ],
     [],
     Block,
     Classes
    };
form_data(navbar, A) -> % {{{2
    [_PID, UlBlock, AllClasses] = admin:maybe_empty(A, 3),
    [Classes, Inverse, Position, Alignment] = admin:maybe_empty(AllClasses, 4),
    Block = case string:tokens(UlBlock, "/") of
         [[$+|B] | _] -> B;
         [B | _] -> B;
        B -> B
    end,

    {[],
     [
      {"Position",
       #dd{
          id=position,
          value=admin:remove_prefix(Position),
          options=position_classes(navbar)
         }
      },
      {"Alignment",
       #dd{
          id=alignment,
          value=admin:remove_prefix(Alignment),
          options=alignment_classes(navbar)
         }
      },
      #checkbox{
         text="Inverse",
         value="inverse",
         label_position=before,
         id=inverse,
         checked=(Inverse == "navbar-inverse")
        }
     ],
     Block,
     Classes
    };
form_data(F, [_, Block, Classes]) -> % {{{2
    {[], [], Block, Classes};
form_data(F, []) -> % {{{2
    {[], []}.

save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, panel, [Block, Header, Addons, Footer, [Classes, Context]]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={bootstrap,
                     panel,
                     [
                      Header,
                      Block,
                      Addons,
                      Footer,
                      [Classes, Context]
                     ]}};
save_block(#cms_mfa{id={PID, Block}, mfa={bootstrap, tab, [TabBlock, Classes]}}=Rec) -> % {{{2
    % Outside blocks
    HeaderBlock = common:sub_block(Block, "tab-header"),
    BodyBlock = common:sub_block(Block, "tab-body"),

    % Inside blocks
    HeaderBlockInside = common:sub_block(TabBlock, "header"),
    BodyBlockInside = common:sub_block(TabBlock, "body"),

    [
     #cms_mfa{
        id={PID, HeaderBlock},
        mfa={bootstrap,
             tab_header,
             [
              HeaderBlockInside,
              Classes
             ]}},
     #cms_mfa{
        id={PID, BodyBlock},
        mfa={bootstrap,
                     tab_body,
                     [
                      BodyBlockInside,
                      Classes
                     ]}}
    ];
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, col, [Block, [Classes, W, O ]]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={bootstrap, col, [Block, W, O, Classes]}};
%save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, full_block, [Block ]}}=Rec) -> % {{{2
%    ColClass = common:q(col_class, ""),
%    Rec#cms_mfa{mfa={bootstrap, full_block, [Block, RowClass, ColClass]}};
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, nav_item, [Block, URL, Text, Classes]}}=Rec) -> % {{{2
    NavItemBlock = common:private_block(common:sub_block(Block, "li")),

    case URL of
        undefined ->
            
            db:maybe_update(#cms_mfa{id={PID, NavItemBlock},
                               mfa={bootstrap, dropdown, [Block]},
                               sort=1}),
            db:maybe_update(#cms_mfa{id={PID, common:sub_block(Block, "link")},
                               mfa={common, text, [Text ++ "<b class='caret'></b>"]},
                               sort=1}),
            [
             Rec#cms_mfa{mfa={bootstrap, nav_item, [NavItemBlock, [Classes]]}}
             %#cms_mfa{id={PID, NavItemBlock},
             %         mfa={bootstrap, dropdown, [Block]},
             %         sort=1},
             %#cms_mfa{id={PID, common:sub_block(Block, "link")},
             %         mfa={common, text, [Text ++ "<b class='caret'></b>"]},
             %         sort=1}
            ];
        URL ->
            db:maybe_update(#cms_mfa{id={PID, NavItemBlock},
                               mfa={common, link_url, [Block, URL]},
                               sort=1}),
            db:maybe_update(#cms_mfa{id={PID, Block},
                               mfa={common, text, [Text]},
                               sort=1}),
            [
             Rec#cms_mfa{mfa={bootstrap, nav_item, [NavItemBlock, Classes]}}
             %#cms_mfa{id={PID, NavItemBlock},
             %         mfa={common, link_url, [Block, URL]},
             %         sort=1},
             %#cms_mfa{id={PID, Block},
             %         mfa={common, text, [Text]},
             %         sort=1}
            ]
    end;
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, navbar, [Block, [Classes | Other]]}}=Rec) -> % {{{2
    NavItemsBlock = common:private_block(common:sub_block(Block, "navbar-ul")),
    Inverse = common:q(inverse, "default"),
    NewClasses = [Classes | admin:prefix_classes(navbar, [Inverse | Other])],

    [
     Rec#cms_mfa{mfa={bootstrap, navbar, [NavItemsBlock, NewClasses]}},
     #cms_mfa{id={PID, NavItemsBlock},
              mfa={bootstrap, nav_items, [Block, ["navbar-nav"]]},
              sort=1}
    ];
save_block(#cms_mfa{id={PID, _}, mfa={M, Fun, [Block, Classes]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={bootstrap, Fun, [Block, Classes]}}.

%% Block renderers {{{1
navbar(Page, Block, Classes) -> % {{{2
    Bindings = [
                {'Block', Block},
                {'BarClasses', Classes}
               ],
    common:template(Page, "templates/navbar.html", Bindings).

nav_items(Page, Block, Classes) -> % {{{2
    common:list(Page, Block, ["nav" | Classes]).

nav_item(Page, ItemID) -> % {{{2
    nav_item(Page, ItemID, []).

nav_item(Page, ItemID, Classes) -> % {{{2
    %<li>
    %<a href="index.html"><i class="fa fa-dashboard fa-fw"></i> Dashboard</a>
    %</li>
    #listitem{
       html_id=common:block_to_html_id(ItemID),
       class=Classes, 
       body=common:parallel_block(Page, ItemID)
      }.

panel(Page, HeaderBlock, BodyBlock, AddonsBlock, FooterBlock, Classes) -> % {{{2
    #panel{
       %html_id=common:block_to_html_id(BodyBlock),
       class=["panel" | Classes],
       body=[
             index:maybe_block(Page, HeaderBlock, ["panel-heading"]),
             index:maybe_block(Page, BodyBlock, ["panel-body"]),
             index:maybe_block(Page, AddonsBlock, []),
             index:maybe_block(Page, FooterBlock, ["panel-footer"])
            ]
      }.

dropdown(Page, Block) -> % {{{2
    LinkBlock = common:sub_block(Block, "link"),
    ItemsBlock = common:sub_block(Block, "items"),
    [
    #link{
       %actions=Event,
       body=common:parallel_block(Page, LinkBlock),
       class="dropdown-toggle",
       html_id=common:block_to_html_id(LinkBlock),
       data_fields=[
                    {toggle, "dropdown"}
                   ]
      },
       common:list(Page, ItemsBlock, ["dropdown-menu"])
    ].

slider(Page, Block, Height, Classes) -> % {{{2
    Bindings = [
                {'Height', "50%"}
               ],
    common:template(Page, "templates/slider.html", Bindings).

full_block(Page, Body) -> % {{{2
    full_block(Page, Body, [], []).

row(Page, Block, Classes) -> % {{{2
    #bs_row{
       html_id=common:block_to_html_id(Block),
       class=Classes,
       body=common:parallel_block(Page, Block)
      }.

col(Page, Block, Width, Offset, Classes) -> % {{{2
    #bs_col{
       html_id=common:block_to_html_id(Block),
       class=Classes,
       cols=column_classes(Width, Offset),
       body=common:parallel_block(Page, Block)
      }.

full_block(Page, Block, RowClasses, ColClasses) -> % {{{2
    #bs_row{
       html_id=common:block_to_html_id(Block),
       class=RowClasses,
       body=#bs_col{
               class=ColClasses,
               cols={lg, 12},
               body=common:parallel_block(Page, Block)
              }}.

tabs(Page, Block, Classes) -> % {{{2
    HeaderBlock = common:sub_block(Block, "tab-header"),
    BodyBlock = common:sub_block(Block, "tab-body"),

    #panel{
       html_id=common:block_to_html_id(Block),
       body=[
             common:list(Page, HeaderBlock, ["nav", "nav-tabs"]),
             #panel{
                class=["tab-content"|Classes],
                body=common:parallel_block(Page, BodyBlock)
               }
            ]}.

tab_header(Page, Block, Classes) -> % {{{2
    [BaseBlock|_] = string:tokens(Block, "/"),
    BodyBlock = common:sub_block(BaseBlock, "body"),
    #listitem{
       html_id=common:block_to_html_id(Block),
       role="presentation",
       class=Classes,
       body=#link{
               url=wf:f("#~s", [common:block_to_html_id(BodyBlock)]),
               %role="tab",
               body=common:parallel_block(Page, Block),
               data_fields=[
                            {toggle, "tab"}
                           ]
              }}.

tab_body(Page, Block, Classes) -> % {{{2

    #panel{
       html_id=common:block_to_html_id(Block),
       %role="tabpanel",
       class=["tab-pane" | Classes],
       body=common:parallel_block(Page, Block)
      }.

%% Event handlers % {{{1
event(submenu) -> % {{{2
    case common:q(submenu, "off") of
        "on" ->
            wf:remove(url_box);
        "off" ->
            wf:insert_after(submenu_box,
                            #panel{
                               id=url_box,
                               body=[
                                     #span{text="URL"},
                                     #txtbx{
                                        id=url,
                                        placeholder="https://yourdomain.com"
                                       }
                                    ]})
    end;
event(Ev) -> % {{{2
    wf:info("~p event ~p", [?MODULE, Ev]).

%% Dropdown formatters {{{1
position_classes(navbar) -> % {{{2
    [
     #option{value=none,
             text="None"},
     #option{value="fixed-top",
             text="Fixed Top"},
     #option{value="fixed-bottom",
             text="Fixed Bottom"},
     #option{value="static-top",
             text="Static Top"}
    ].

alignment_classes(navbar) -> % {{{2
    [
     #option{value=none,
             text="None"},
     #option{value="left",
             text="Left"},
     #option{value="right",
             text="Right"}
    ].

context_classes(F) -> % {{{2
    [
     #option{value=wf:f("~s-default", [F]),
             text="Default"},
     #option{value=wf:f("~s-primary", [F]),
             text="Primary"},
     #option{value=wf:f("~s-success", [F]),
             text="Success"},
     #option{value=wf:f("~s-info", [F]),
             text="Info"},
     #option{value=wf:f("~s-warning", [F]),
             text="Warning"},
     #option{value=wf:f("~s-danger", [F]),
             text="Danger"}
    ].
assets_dropdown(AssetType) -> % {{{2
    AssetsDup = db:get_assets(AssetType),
    Assets = sets:to_list(sets:from_list(AssetsDup)),
    Options = [ #option{value=string:join(Id, "."),
                        text=Name} || #{id := Id,
                                        name := Name} <- Assets],
     #dd{
        id=asset_id,
        options=Options
       }.

%% Helpers {{{1
get_navitem_data(_PID, "") -> % {{{ 2
    {"", "", ""};
get_navitem_data(PID, NavBlock) -> % {{{ 2
    [Block, "li"] = string:tokens(NavBlock, "/"),
    {Text, URL} = case db:get_mfa(PID, NavBlock) of
                      [#cms_mfa{mfa={_, dropdown, [Block]}}] -> 
                          [#cms_mfa{mfa={_, _, [T]}}] = db:get_mfa(PID, common:sub_block(Block, "link")),
                          {string:substr(T, 1, length(T) - 21), ""};
                      [#cms_mfa{mfa={_, link_url, [Block, U]}}] -> 
                          [#cms_mfa{mfa={_, _, [T]}}] = db:get_mfa(PID, Block),
                          {T, U}
                  end,
    {Block, Text, URL}.

column_classes(Width, "") -> % {{{2
    W = list_to_integer(Width),
    if W > 0, W =< 12 ->
           [wf:f("col-~s-~s", [Screen, Width]) || Screen <- ["xs", "sm", "md", "lg"]];
       true -> column_classes("12", "")
    end;
column_classes(Width, Offset) -> % {{{2
      O = list_to_integer(Offset),
      if O > 0, O =< 12 ->
             [wf:f("col-~s-offset-~s", [Screen, Offset]) || Screen <- ["xs", "sm", "md", "lg"]] ++ column_classes(Width, "");
         true -> column_classes(Width, "")
      end;
column_classes(_, _) -> % {{{2
    column_classes("12", "").

