-module(bootstrap).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").
-include("records.hrl").

?DESCRIPTION(Bootstrap).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
     {navbar, "Navigation Bar"},
     {nav_item, "Navigation Bar button"},
     {panel, "Panel"},
     {modal, "Modal"},
     {slider, "Slider"},
     %{dropdown, "Dropdown"},
     %{script, "Inline Script"},
     {container, "Container"},
     {tabs, "Tabs"},
     {tab, "Tab"},
     {row, "Row"},
     {col, "Column"},
     {table, "Table"},
     {th, "Table header"},
     {tr, "Table row"},
     {td, "Table cell"}
     %{full_block, "One Column Row"}
     ].

format_block(col, [Block, Width, Offset, Classes]) -> % {{{2
    {wf:f("Column: ~s(width=~p, offset=~p, classes=~p)",
         [Block, Width, Offset, Classes]),
    Block};
format_block(panel, [BodyBlock, HeaderBlock, AddonsBlock, FooterBlock, Classes]) -> % {{{2
    format_block(panel, [BodyBlock, HeaderBlock, AddonsBlock, FooterBlock, Classes, []]);
format_block(panel, [BodyBlock, HeaderBlock, AddonsBlock, FooterBlock, Classes, DataAttr]) -> % {{{2
    format_block(panel, [BodyBlock, HeaderBlock, AddonsBlock, FooterBlock, [],[],[],[], Classes, DataAttr]);
format_block(panel, [BodyBlock, HeaderBlock, AddonsBlock, FooterBlock, HeaderCls, BodyCls, AddonCls, FooterCls, Classes, DataAttr]) -> % {{{2
    {wf:f("Panel(header:=~p, body:=~p, addons:=~p, footer:=~p, SubClasses=(~p~p~p~p), classes=~p, attr:~p)",
         [HeaderBlock, BodyBlock, AddonsBlock, FooterBlock, HeaderCls, BodyCls, AddonCls, FooterCls, Classes, DataAttr]),
     BodyBlock};
format_block(modal, [Block, HeaderBlock, BodyBlock, FooterBlock, Classes, _DataAttr]) -> % {{{2
    {wf:f("Modal(link_block=~p, title_block=~p, body_block=~p, footer_block=~p, classes=~p)",
         [Block, HeaderBlock, BodyBlock, FooterBlock, Classes]),
     Block};
format_block(full_block, [Block, RowClass, ColClass]) -> % {{{2
    {wf:f("One Column Row: ~s(row_class=~p, col_class=~p)",
         [Block, RowClass, ColClass]),
    Block};
format_block(navbar, [Block, Classes]) -> % {{{2
    [B| _] = string:tokens(common:format_private_block(Block), "/"),

    {wf:f("NavBar: ~s(class=~p)", [B, Classes]), B};
format_block(nav_item, [Block, Classes, DataAttr]) -> % {{{2
    [B, _] = string:tokens(common:format_private_block(Block), "/"),
    {wf:f("NavBarButton: ~s(class=~p, attr:~p)", [B, Classes, DataAttr]), B};
format_block(container, [Block, AllClasses]) -> % {{{2
    [Fluid, Classes] = admin:maybe_empty(AllClasses, 2),
    IsFluid = (Fluid == "container-fluid"),
    {wf:f("Container: ~s(fluid=~p, class=~p)", [Block, IsFluid, Classes]), Block};
format_block(F, [Block, Classes]) -> % {{{2
    {wf:f("bootstrap:~s(id=~p, classes=~p)", [F,Block, Classes]), Block};
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
    [_, Block, HeaderBlock,  AddonsBlock, FooterBlock, HeaderCls, BodyCls, FooterCls, AddonCls, Classes0, DataAttr] = admin:maybe_empty(A, 11),
    [Classes, Context] = admin:maybe_empty(Classes0, 2),
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
         }},
      {"Header Classes",
       #txtbx{
         id=panel_header_cls,
         text=HeaderCls,
         placeholder="Header classes (leave blank for none)"
        }},
      {"Body Classes",
       #txtbx{
         id=panel_body_cls,
         text=BodyCls,
         placeholder="Body classes (leave blank for none)"
        }},
      {"Addons Classes",
       #txtbx{
         id=panel_addons_cls,
         text=AddonCls,
         placeholder="Addons classes (leave blank for none)"
        }},
      {"Footer Classes",
       #txtbx{
         id=panel_footer_cls,
         text=FooterCls,
         placeholder="Footer classes (leave blank for none)"
        }}
     ],
     Block,
     Classes,
     DataAttr
    };
form_data(modal, A) -> % {{{2
    [_, Block, HeaderBlock, BodyBlock, FooterBlock, Classes, DataAttr] = admin:maybe_empty(A, 7),
    %[Classes, Context] = admin:maybe_empty(C, 2),
    {[
      {"Block for modal title",
      #txtbx{
         id=header_block,
         text=HeaderBlock,
         placeholder="Block name for modal title (leave blank for none)"
        }},
      {"Block for modal body",
      #txtbx{
         id=body_block,
         text=BodyBlock,
         placeholder="Block name for modal body"
        }},
    {"Block for modal footer",
      #txtbx{
         id=footer_block,
         text=FooterBlock,
         placeholder="Block name for modal title (leave blank for none)"
        }}
     ],
     [],
     Block,
     Classes
     ,
     DataAttr
    };
form_data(nav_item, A) -> % {{{2
    [PID, NavBlock, Classes, DataAttr] = admin:maybe_empty(A, 4),
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
     Classes,
     DataAttr
    };
form_data(navbar, A) -> % {{{2
    [PID, NavItemsBlock, AllClasses] = admin:maybe_empty(A, 3),
    [Classes, Inverse, Position] = admin:maybe_empty(AllClasses, 3),


    {Block, Alignment} = get_navbar_alignmant(PID, NavItemsBlock),
    ?LOG("NavBar: ~p ~p", [Position, Alignment]),
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
form_data(container, A) -> % {{{2
    [_PID, Block, AllClasses] = admin:maybe_empty(A, 3),
    [Fluid, Classes] = admin:maybe_empty(AllClasses, 2),
    {[],
     [
      #checkbox{
         text="Fluid",
         value="fluid",
         label_position=before,
         id=fluid,
         checked=(Fluid == "container-fluid")
        }
     ],
     Block,
     Classes
    };
form_data(_F, [_, Block, Classes]) -> % {{{2
    {[], [], Block, Classes};
form_data(_F, []) -> % {{{2
    {[], []}.

save_block(#cms_mfa{mfa={bootstrap, panel, [Header, Block, Addons, Footer, [Classes], DataAttr]}}=Rec) -> % {{{2
    Context = common:q(context, ""),
    HeaderCls = common:q(panel_header_cls, ""),
    BodyCls = common:q(panel_body_cls, ""),
    AddonCls = common:q(panel_addons_cls, ""),
    FooterCls = common:q(panel_footer_cls, ""),
    Rec#cms_mfa{mfa={bootstrap,
                     panel,
                     [
                      Header,
                      Block,
                      Addons,
                      Footer,
                      HeaderCls, BodyCls, FooterCls, AddonCls,
                      [Classes, Context],
                      DataAttr
                     ]}};
save_block(#cms_mfa{id={PID, Block}, mfa={bootstrap, tab, [TabBlock, Classes, _DataAttr]}}=Rec) -> % {{{2
    ?LOG("~nsave_tab: ~p", [Rec]),
    Sort = Rec#cms_mfa.sort,
    % Outside blocks
    HeaderBlock = common:sub_block(Block, "tab-header"),
    BodyBlock = common:sub_block(Block, "tab-body"),

    % Inside blocks
    HeaderBlockInside = common:sub_block(TabBlock, "header"),
    BodyBlockInside = common:sub_block(TabBlock, "body"),

    db:fix_sort([
                 #cms_mfa{
                    id={PID, HeaderBlock},
                    mfa={bootstrap,
                         tab_header,
                         [
                          HeaderBlockInside,
                          Classes
                         ]},
                    sort=Sort},
                 #cms_mfa{
                    id={PID, BodyBlock},
                    mfa={bootstrap,
                         tab_body,
                         [
                          BodyBlockInside,
                          Classes
                         ]},
                    sort=Sort}
                ]);
save_block(#cms_mfa{id={_PID, _}, mfa={bootstrap, col, [Block, [Classes, W, O ], _DataAttr]}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={bootstrap, col, [Block, W, O, Classes]}};
%save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, full_block, [Block ]}}=Rec) -> % {{{2
%    ColClass = common:q(col_class, ""),
%    Rec#cms_mfa{mfa={bootstrap, full_block, [Block, RowClass, ColClass]}};
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, nav_item, [Block, URL, Text, Classes, DataAttr]}}=Rec) -> % {{{2
    NavItemBlock = common:private_block(common:sub_block(Block, "li")),

    case URL of
        "" ->
            
            db:maybe_update(#cms_mfa{id={PID, NavItemBlock},
                               mfa={bootstrap, dropdown, [Block]},
                               sort=1}),
            db:maybe_update(#cms_mfa{id={PID, common:sub_block(Block, "link")},
                               mfa={common, text, [Text ++ "<b class='caret'></b>"]},
                               sort=1}),
            [
             Rec#cms_mfa{mfa={bootstrap, nav_item, [NavItemBlock, [Classes], DataAttr]}}
            ];
        URL ->
            db:maybe_update(#cms_mfa{id={PID, NavItemBlock},
                               mfa={common, link_url, [Block, URL]},
                               sort=1}),
            db:maybe_update(#cms_mfa{id={PID, Block},
                               mfa={common, text, [Text]},
                               sort=1}),
            [
             Rec#cms_mfa{mfa={bootstrap, nav_item, [NavItemBlock, Classes, DataAttr]}}
             %#cms_mfa{id={PID, NavItemBlock},
             %         mfa={common, link_url, [Block, URL]},
             %         sort=1},
             %#cms_mfa{id={PID, Block},
             %         mfa={common, text, [Text]},
             %         sort=1}
            ]
    end;
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, navbar, [Block, [Classes, Position, Padding], _DataAttr]}}=Rec) -> % {{{2
    NavItemsBlock = common:private_block(common:sub_block(Block, "navbar-ul")),
    Inverse = common:q(inverse, "default"),
    NewClasses = [Classes | admin:prefix_classes(navbar, [Inverse, Position])],
    PPadding = admin:prefix_classes(navbar, [Padding]),
    ?LOG("Prefix: ~p ~p", [NewClasses, PPadding]),

    db:maybe_update(#cms_mfa{id={PID, NavItemsBlock},
                             mfa={bootstrap, nav_items, [Block, ["navbar-nav" | PPadding]]},
                             sort=1}),
    Rec#cms_mfa{mfa={bootstrap, navbar, [NavItemsBlock, NewClasses]}};
save_block(#cms_mfa{id={_PID, _}, mfa={bootstrap, container, [Block, Classes, _DataAttr]}}=Rec) -> % {{{2
    Fluid = case common:q(fluid, "") of
                "" -> "container";
                "fluid" -> "container-fluid"
            end,
    Rec#cms_mfa{mfa={bootstrap, container, [Block, [Fluid | Classes]]}};
save_block(#cms_mfa{id={_PID, _}, mfa={_M, Fun, [Block, Classes, _DataAttr]}}=Rec) -> % {{{2 
    ?LOG("~nsave_block: ~p", [Rec]),
    wf:warning("Bootstrap: ~p", [Rec]),
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
    nav_item(Page, ItemID, Classes, []).
nav_item(Page, ItemID, Classes, DataAttr) -> % {{{2
    %<li>
    %<a href="index.html"><i class="fa fa-dashboard fa-fw"></i> Dashboard</a>
    %</li>
    #listitem{
       html_id=common:block_to_html_id(ItemID),
       class=Classes, 
       body=common:parallel_block(Page, ItemID),
       data_fields = DataAttr
      }.
panel(Page, BodyBlock, HeaderBlock,  AddonsBlock, FooterBlock, Classes) -> % {{{2
    panel(Page,  HeaderBlock, BodyBlock, AddonsBlock, FooterBlock, Classes, []).
panel(Page, BodyBlock, HeaderBlock,  AddonsBlock, FooterBlock, Classes, DataAttr) -> % {{{2
    panel(Page,  HeaderBlock, BodyBlock, AddonsBlock, FooterBlock, [],[],[],[], Classes,  DataAttr).
panel(Page, BodyBlock, HeaderBlock, AddonsBlock, FooterBlock, HeaderCls, BodyCls, FooterCls, AddonCls, Classes, DataAttr) -> % {{{2
    #panel{
       class=["panel" | Classes],
       body=[
             index:maybe_block(Page, HeaderBlock, [HeaderCls , "panel-heading"]),
             index:maybe_block(Page, BodyBlock, [BodyCls , "panel-body"]),
             index:maybe_block(Page, AddonsBlock, [AddonCls]),
             index:maybe_block(Page, FooterBlock, [FooterCls , "panel-footer"])
            ],
       data_fields = DataAttr
      }.

dropdown(Page, Block) -> % {{{2
    LinkBlock = common:sub_block(Block, "link"),
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
       common:list(Page, Block, ["dropdown-menu"])
    ].

slider(Page, Block, Classes) -> % {{{2
    slider(Page, Block, 5000, Classes).

slider(Page, Block, Interval, Classes) -> % {{{2
    Slides = common:parallel_block(Page, Block),
    NSlides = length(Slides),
    Id = common:block_to_html_id(Block),
    wf:wire(#script{script=wf:f("$('#~s').carousel({interval: ~p});", [Id, Interval])}),
    #html5_header{
       html_id=Id,
       class=["carousel", "slide" | Classes],
       body=[
             carusel_indicators(Block, NSlides),
             carusel_items(Slides),
             carusel_controls(Block)
            ]}.

full_block(Page, Body) -> % {{{2
    full_block(Page, Body, [], []).

container(Page, Block, Classes) -> % {{{2
    common:block(Page, Block, Classes).

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

table(Page, Block,Classes) -> % {{{2
    #table { 
      html_id=common:block_to_html_id(Block),
      rows=common:parallel_block(Page, Block),
      class=Classes
    }.

tr(Page, Block, Classes) -> % {{{2 
    #tablerow { 
      html_id=common:block_to_html_id(Block),
      class=Classes,
      cells=common:parallel_block(Page, Block)
    }.

th(Page, Block, Classes) -> % {{{2 
    #tableheader { 
      html_id=common:block_to_html_id(Block),
      class=Classes,
      body=common:parallel_block(Page, Block)
    }.

td(Page, Block, Classes) -> % {{{2 
    #tablecell { 
      html_id=common:block_to_html_id(Block),
      class=Classes,
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
    tabs(Page, Block, Classes, []).
tabs(Page, Block, Classes, DataAttr) -> % {{{2
    HeaderBlock = common:sub_block(Block, "tab-header"),
    BodyBlock = common:sub_block(Block, "tab-body"),

    #panel{
       html_id=common:block_to_html_id(Block),
       body=[
             common:list(Page, HeaderBlock, ["nav", "nav-tabs"]),
             #panel{
                class=["tab-content"|Classes],
                body=common:parallel_block(Page, BodyBlock),
                data_fields = DataAttr
               }
            ]}.
tab_header(Page, Block, Classes) -> % {{{2
    tab_header(Page, Block, Classes, []).
tab_header(Page, Block, Classes, _DataAttr) -> % {{{2
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

modal(Page, Block, TitleBlock, BodyBlock, FooterBlock, Classes) -> % {{{2
    modal(Page, Block, TitleBlock, BodyBlock, FooterBlock, Classes, []).
modal(Page, Block, TitleBlock, BodyBlock, FooterBlock, Classes, _DataAttr) -> % {{{2
    common:link_event(Page, Block, ?POSTBACK({modal, Page, TitleBlock, BodyBlock, FooterBlock}, ?MODULE), Classes).

%% Event handlers % {
event({modal, Page, TitleBlock, BodyBlock, FooterBlock}) -> % {{{2
    Title = common:parallel_block(Page, TitleBlock),
    Body = common:parallel_block(Page, BodyBlock),
    Bottom = common:parallel_block(Page, FooterBlock),
    coldstrap:modal(Title, Body, Bottom, [{has_x_button, true}]);
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
    ?LOG("~p event ~p", [?MODULE, Ev]).

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
    [Block, "li"] = string:tokens(common:format_private_block(NavBlock), "/"),
    {Text, URL} = case db:get_mfa(PID, NavBlock) of
                      [#cms_mfa{mfa={_, dropdown, [Block]}}] -> 
                          [#cms_mfa{mfa={_, _, [T]}}] = db:get_mfa(PID, common:sub_block(Block, "link")),
                          {string:substr(T, 1, length(T) - 21), ""};
                      [#cms_mfa{mfa={_, link_url, [Block, U]}}] -> 
                          [#cms_mfa{mfa={_, _, [T]}}] = db:get_mfa(PID, Block),
                          {T, U}
                  end,
    {Block, Text, URL}.

column_classes("", "") -> % {{{2
    [];
column_classes("0", "") -> % {{{2
    [];
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
      end.

carusel_indicators(Block, NSlides) when NSlides > 0 -> % {{{2
    #list{
       class=["carousel-indicators"],
       numbered=true,
       body=[#listitem{
                data_fields=[
                             {target, wf:f("#~s", [common:block_to_html_id(Block)])},
                             {"slide-to", N}]}
             || N <- lists:seq(0, NSlides - 1)]};
carusel_indicators(_Block, 0) -> % {{{2
    "".

carusel_items(Slides) -> % {{{2
    #panel{
       class=["carousel-inner"],
       body=Slides
      }.
       
carusel_controls(Block) -> % {{{2
    [
     #link{
        url=wf:f("#~s", [common:block_to_html_id(Block)]),
        class=["left", "carousel-control"],
        data_fields=[{slide, "prev"}],
        body=[
              #span{class=["icon-prev"]}
             ]},
     #link{
        url=wf:f("#~s", [common:block_to_html_id(Block)]),
        class=["right", "carousel-control"],
        data_fields=[{slide, "next"}],
        body=[
              #span{class=["icon-next"]}
             ]}
    ].

get_navbar_alignmant(PID, Block) -> % {{{2
    case db:get_mfa(PID, Block) of
        [#cms_mfa{mfa={bootstrap, nav_items, [SBlock, [_, Alignment]]}}|_] ->
            {SBlock, Alignment};
        [] -> {"", ""};
        A ->
            wf:warning("Wrong alignment: ~p", [A]),
            {"", []}
    end.
