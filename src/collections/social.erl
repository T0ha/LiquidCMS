-module(social).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("db.hrl").
-include("cms.hrl").
-include("records.hrl").

?DESCRIPTION(Social elements).

%% CMS Module interface {{{1
functions() -> % {{{2
    [
        {vk_share, "Vk share"},
        {fb_share, "Facebook share"},
        {twitter_share, "Twitter share"},
        {google_share, "Google plus share"},
        {telegram_share, "Telegram share"},
        {pinterest_share, "Pinterest share"},
        {pocket, "Pocket"},
        {linkedIn, "LinkedIn Share"}
    ].


format_block(F, [Block|_]=A) -> % {{{2
    {wf:f("~p:~s(~p)", [?MODULE, F, A]), Block}.

form_data(_F, A) -> % {{{2
    [_, Block, Title, Classes, DataAttr] = admin:maybe_empty(A, 5),
    {[
       {"Title",
       #txtbx{
          id=title,
          text=Title,
          placeholder="title"
         }}
     ],
     [], Block, Classes, DataAttr}.

save_block(#cms_mfa{mfa={?MODULE, F, A}}=Rec) -> % {{{2
    Rec#cms_mfa{mfa={?MODULE, F, A}}.

vk_share(Page, Block, Title, Classes, DataAttr) ->  % {{{2
    #link{
        url="https://vk.com/share.php?url="++wf:url(), 
        html_id=common:block_to_html_id(Block),
        class="btn btn-social-icon btn-vk "++Classes,
        title=Title,
        new=true,
        body = #span{
            class="fa fa-vk",
            body=common:parallel_block(Page, Block)
        },
        data_fields = DataAttr
    }.

fb_share(Page, Block, Title, Classes, DataAttr) ->  % {{{2
    #link{
        url="https://www.facebook.com/sharer.php?u="++wf:url(), 
        html_id=common:block_to_html_id(Block),
        class="btn btn-social-icon btn-facebook "++Classes,
        title=Title,
        new=true,
        body = #span{
            class="fa fa-facebook",
            body=common:parallel_block(Page, Block)
        },
        data_fields = DataAttr
    }.

twitter_share(Page, Block, Title, Classes, DataAttr) ->  % {{{2
    #link{
        url="https://twitter.com/intent/tweet?url="++wf:url(), 
        html_id=common:block_to_html_id(Block),
        class="btn btn-social-icon btn-twitter "++Classes,
        title=Title,
        new=true,
        body = #span{
            class="fa fa-twitter",
            body=common:parallel_block(Page, Block)
        },
        data_fields = DataAttr
    }.

google_share(Page, Block, Title, Classes, DataAttr) ->  % {{{2
    #link{
        url="https://plus.google.com/share?url="++wf:url(), 
        html_id=common:block_to_html_id(Block),
        class="btn btn-social-icon btn-google "++Classes,
        title=Title,
        new=true,
        body = #span{
            class="fa fa-google",
            body=common:parallel_block(Page, Block)
        },
        data_fields = DataAttr
    }.

telegram_share(Page, Block, Title, Classes, DataAttr) ->  % {{{2
    #link{
        url="https://telegram.me/share/url?url="++wf:url(), 
        html_id=common:block_to_html_id(Block),
        class="btn btn-social-icon btn-telegram "++Classes,
        title=Title,
        new=true,
        body = #span{
            class="fa fa-telegram",
            body=common:parallel_block(Page, Block)
        },
        data_fields = DataAttr
    }.

pinterest_share(Page, Block, Title, Classes, DataAttr) ->  % {{{2
    #link{
        url="https://pinterest.com/pin/create/button/?url="++wf:url(), 
        html_id=common:block_to_html_id(Block),
        class="btn btn-social-icon btn-pinterest "++Classes,
        title=Title,
        new=true,
        body = #span{
            class="fa fa-pinterest",
            body=common:parallel_block(Page, Block)
        },
        data_fields = DataAttr
    }.

pocket(Page, Block, Title, Classes, DataAttr) ->  % {{{2
    #link{
        url="https://getpocket.com/save?url="++wf:url(), 
        html_id=common:block_to_html_id(Block),
        class="btn btn-social-icon btn-pocket pocket-btn "++Classes,
        title=Title,
        new=true,
        body = #span{
            class="fa fa-pocket",
            body=common:parallel_block(Page, Block)
        },
        data_fields=DataAttr
    }.

linkedIn(Page, Block, Title, Classes, DataAttr) ->  % {{{2
    #link{
        url="https://www.linkedin.com/shareArticle?mini=true&url="++wf:url(), 
        html_id=common:block_to_html_id(Block),
        class="btn btn-social-icon btn-linkedin "++Classes,
        title=Title,
        new=true,
        body = #span{
            class="fa fa-linkedin",
            body=common:parallel_block(Page, Block)
        },
        data_fields = DataAttr
    }.
