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
     {slider, "Slider"},
     %{nav_items, "Navigation Menu Items List"},
     %{script, "Inline Script"},
     {full_block, "One Column Row"}
     ].

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
    wf:f("common:~s(~p)", [F, A]).

form_data(nav_item) -> % {{{2
    [
     #span{text="Block for button"},
     #txtbx{
        id=block,
        text="navbar-button",
        placeholder="Block name for navbar elements"
       },
     #span{text="URL"},
     #txtbx{
        id=url,
        placeholder="https://yourdomain.com"
       },
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
     #span{text="Additional CSS classes for NavBar"}
    ];
form_data(F) -> % {{{2
    [].

save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, nav_item, []}}=Rec) -> % {{{2
    Block = common:q(block, "navbar-button"),
    URL = common:q(url, "https://liquid-nitrogen.org"),
    Text = common:q(text, "Just button"),
    NavItemBlock = common:sub_block(Block, "li"),
    [
     Rec#cms_mfa{mfa={bootstrap, nav_item, [NavItemBlock]}},
     #cms_mfa{id={PID, NavItemBlock},
              mfa={common, link_url, [Block, URL]},
              sort=1},
     #cms_mfa{id={PID, Block},
              mfa={common, text, [Text]},
              sort=1}
    ];
save_block(#cms_mfa{id={PID, _}, mfa={bootstrap, navbar, []}}=Rec) -> % {{{2
    Block = common:q(nav_block, "navbar-main"),
    NavItemsBlock = common:sub_block(Block, "navbar-ul"),
    [
     Rec#cms_mfa{mfa={bootstrap, navbar, [NavItemsBlock, []]}},
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
    Items = common:parallel_block(Page, Block),
    #list{body=Items, class=["nav" | Classes], html_id=Block}.

nav_item(Page, ItemID) -> % {{{2
    %<li>
    %<a href="index.html"><i class="fa fa-dashboard fa-fw"></i> Dashboard</a>
    %</li>
    #listitem{body=common:parallel_block(Page, ItemID)}.

full_block(_Page, Body) -> % {{{2
    #bs_row{
       body=#bs_col{
               cols={lg, 12},
               body=Body}}.


%% Event handlers % {{{1
event(Ev) -> % {{{2
    wf:info("~p event ~p", [?MODULE, Ev]).

%% Helpers {{{1

%% Dropdown formatters {{{1
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
