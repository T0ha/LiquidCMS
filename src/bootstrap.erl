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
     {slider, "Slider"},
     %{script, "Inline Script"},
     {full_block, "One Column Row"}
     ].

format_block(panel, [HeaderBlock, BodyBlock]) -> % {{{2
    wf:f("Panel: ~s(header_block=~p, body_block=~p)",
         [HeaderBlock, BodyBlock]);
format_block(full_block, [Block, RowClass, ColClass]) -> % {{{2
    wf:f("One Column Row: ~s(row_class=~p, col_class=~p)",
         [Block, RowClass, ColClass]);
format_block(navbar, [Block, Classes]) -> % {{{2
    wf:f("NavBar: ~s(class=~p)", [Block, Classes]);
format_block(nav_item, [Block]) -> % {{{2
    wf:f("NavBarButton: ~s", [Block]);
format_block(template, [TID]) -> % {{{2
    [#cms_template{name=Name}] = db:get_template(TID),
    wf:f("Template: ~s(~p)", [Name, TID]);
format_block(asset, [AID]) -> % {{{2
    [#cms_asset{type=Type, file=File, name=Name}|_] = db:get_asset(AID),
    wf:f("Asset ~s: ~s(~p)", [Type, Name, File]);
format_block(F, A) -> % {{{2
    wf:f("bootstrap:~s(~p)", [F, A]).

form_data(panel) -> % {{{2
    [
     #span{text="Block for panel header"},
     #txtbx{
        id=panel_header_block,
        text="",
        placeholder="Block name for panel header (leave blank for none)"
       },
     #span{text="Block for panel body"},
     #txtbx{
        id=panel_body_block,
        text="panel-body",
        placeholder="Block name for panel body elements"
       },
     #span{text="Block for panel addons"},
     #txtbx{
        id=panel_addons_block,
        text="",
        placeholder="Block name for panel addons (leave blank for none)"
       },
     #span{text="Block for panel header"},
     #txtbx{
        id=panel_footer_block,
        text="",
        placeholder="Block name for panel footer (leave blank for none)"
       },
     #span{text="Formatting"},
     #bs_row{
        body=[
              #bs_col{
                 cols={lg, 3},
                 body=[
                       #span{text="Context"},
                       #dd{
                          id=context,
                          options=context_classes(panel)
                         }
                      ]},
              #bs_col{
                 cols={lg, 3},
                 body=[
                       #span{text="Custom classes"},
                       #txtarea{
                          id=css_classes,
                          placeholder="Other CSS classes"
                         }
                      ]}
             ]}
    ];
form_data(full_block) -> % {{{2
    [
     #span{text="Block name"},
     #txtbx{
        id=block,
        text="page-row",
        placeholder="Block name for horizontal block"
       },
     #span{text="Formatting"},
     #bs_row{
        body=[
              #bs_col{
                 cols={lg, 6},
                 body=[
                       #span{text="Additional classes for .row div"},
                       #txtarea{
                          id=row_class,
                          placeholder="Additional CSS classes"
                         }
                      ]},
              #bs_col{
                 cols={lg, 6},
                 body=[
                       #span{text="Additional classes for .col div"},
                       #txtarea{
                          id=col_class,
                          placeholder="Additional CSS classes"
                         }
                      ]}
             ]}
    ];
form_data(slider) -> % {{{2
    [
     #span{text="Block for slider"},
     #txtbx{
        id=block,
        text="navbar-main",
        placeholder="Block name for navbar elements"
       },
     #span{text="Formatting"},
     #bs_row{
        body=[
              #bs_col{
                 cols={lg, 3},
                 body=[
                       #span{text="Position"},
                       #dd{
                          id=position,
                          options=position_classes(navbar)
                         }
                      ]},
              #bs_col{
                 cols={lg, 3},
                 body=[
                       #span{text="Alignment"},
                       #dd{
                          id=alignment,
                          options=alignment_classes(navbar)
                         }
                      ]},
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
                      ]},
              #bs_col{
                 cols={lg, 3},
                 body=[
                       #span{text="Custom classes"},
                       #txtarea{
                          id=css_classes,
                          placeholder="Other CSS classes"
                         }
                      ]}
             ]}
    ];
form_data(nav_item) -> % {{{2
    [
     #span{text="Block for button"},
     #txtbx{
        id=block,
        text="navbar-button",
        placeholder="Block name for navbar elements"
       },
     #panel{
        id=submenu_box,
        body=
        #checkbox{
           text="Dropdown",
           id=submenu,
           postback=submenu,
           delegate=?MODULE,
           checked=false
          }
       },
     #br{},
     #span{text="Text"},
     #txtbx{
        id=text,
        placeholder="Text for button"
       }
    ];
form_data(navbar) -> % {{{2
    [
     #span{text="Block for navbar"},
     #txtbx{
        id=nav_block,
        text="navbar-main",
        placeholder="Block name for navbar elements"
       },
     #span{text="Formatting"},
     #bs_row{
        body=[
              #bs_col{
                 cols={lg, 3},
                 body=[
                       #span{text="Position"},
                       #dd{
                          id=position,
                          options=position_classes(navbar)
                         }
                      ]},
              #bs_col{
                 cols={lg, 3},
                 body=[
                       #span{text="Alignment"},
                       #dd{
                          id=alignment,
                          options=alignment_classes(navbar)
                         }
                      ]},
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
                      ]},
              #bs_col{
                 cols={lg, 3},
                 body=[
                       #span{text="Custom classes"},
                       #txtarea{
                          id=css_classes,
                          placeholder="Other CSS classes"
                         }
                      ]}
             ]}
    ];
form_data(F) -> % {{{2
    [].

save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, panel, []}}=Rec) -> % {{{2
    HeaderBlock = common:q(panel_header_block, ""),
    FooterBlock = common:q(panel_footer_block, ""),
    BodyBlock = common:q(panel_body_block, "panel-body"),
    AddonsBlock = common:q(panel_addons_block, ""),
    
    Classes = get_classes("panel"),
    Rec#cms_mfa{mfa={bootstrap, panel, [HeaderBlock, BodyBlock, AddonsBlock, FooterBlock, Classes]}};
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, full_block, []}}=Rec) -> % {{{2
    Block = common:q(block, "page-row"),
    RowClass = common:q(row_class, ""),
    ColClass = common:q(col_class, ""),
    Rec#cms_mfa{mfa={bootstrap, full_block, [Block, RowClass, ColClass]}};
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, nav_item, []}}=Rec) -> % {{{2
    Block = common:q(block, "navbar-button"),
    Text = common:q(text, "Just button"),
    NavItemBlock = common:sub_block(Block, "li"),

    case common:q(url, undefined) of
        undefined ->
            [
             Rec#cms_mfa{mfa={bootstrap, nav_item, [NavItemBlock]}},
             #cms_mfa{id={PID, NavItemBlock},
                      mfa={bootstrap, dropdown, [Block]},
                      sort=1},
             #cms_mfa{id={PID, common:sub_block(Block, "link")},
                      mfa={common, text, [Text ++ "<b class='caret'></b>"]},
                      sort=1}
            ];
        URL ->
            %URL = common:q(url, "https://liquid-nitrogen.org"),
            [
             Rec#cms_mfa{mfa={bootstrap, nav_item, [NavItemBlock]}},
             #cms_mfa{id={PID, NavItemBlock},
                      mfa={common, link_url, [Block, URL]},
                      sort=1},
             #cms_mfa{id={PID, Block},
                      mfa={common, text, [Text]},
                      sort=1}
            ]
    end;
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, navbar, []}}=Rec) -> % {{{2
    Block = common:q(nav_block, "navbar-main"),
    NavItemsBlock = common:sub_block(Block, "navbar-ul"),
    Classes = get_classes("navbar"),
    [
     Rec#cms_mfa{mfa={bootstrap, navbar, [NavItemsBlock, Classes]}},
     #cms_mfa{id={PID, NavItemsBlock},
              mfa={bootstrap, nav_items, [Block, ["navbar-nav"]]},
              sort=1}
    ].

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
    %<li>
    %<a href="index.html"><i class="fa fa-dashboard fa-fw"></i> Dashboard</a>
    %</li>
    #listitem{body=common:parallel_block(Page, ItemID)}.

panel(Page, HeaderBlock, BodyBlock, AddonsBlock, FooterBlock, Classes) -> % {{{2
    #panel{
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

full_block(Page, Block, RowClasses, ColClasses) -> % {{{2
    #bs_row{
       class=RowClasses,
       body=#bs_col{
               class=ColClasses,
               cols={lg, 12},
               body=common:parallel_block(Page, Block)
              }}.


%% Event handlers % {{{1
event(submenu) -> % {{{2
    case common:q(submenu, "off") of
        "off" ->
            wf:remove(url_box);
        "on" ->
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

%% Helpers {{{1

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

context_classes(_) -> % {{{2
    [
     #option{value="default",
             text="Default"},
     #option{value="primary",
             text="Primary"},
     #option{value="success",
             text="Success"},
     #option{value="info",
             text="Info"},
     #option{value="warning",
             text="Warning"},
     #option{value="danger",
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
get_classes(Prefix) -> % {{{2
   AllClasses = wf:mq([position, alignment, inverse, context, css_classes]),
   Classes = sets:to_list(sets:from_list(AllClasses)),
   lists:map(fun(none) -> "";
                (undefined) -> wf:f("~s-default", [Prefix]);
                ("") -> "";
                (Class) -> wf:f("~s-~s", [Prefix, Class])
            end,
            Classes).
