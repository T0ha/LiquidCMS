-define(DB_PREFIX(), liquid_cms_).
-define(POSTBACK(P), #event{
                        type=click,
                        postback=P
                       }).
-define(POSTBACK(P, Delegate), #event{
                        type=click,
                        postback=P,
                        delegate=Delegate
                       }).
-type role() :: 'admin' | 'editor' | undefined.
-type asset_type() :: 'image' | 'script' | 'css' | 'less' | binary.

%% DB tables records goes here
-record(cms_mfa, {
          id :: {string(), string()},
          sort :: non_neg_integer(),
          mfa :: {module(), fun(), [any()]} | fun(),
          settings :: map()
         }).

-record(cms_template, {
          %id :: binary(),
          file :: iodata(), % Path
          bindings=[] :: [proplists:property()],
          name="" :: string(),
          description="" :: string(),
          settings=#{} :: map()
         }).

-record(cms_block_template, {
          id :: {string(), string()},
          template_id :: binary(),
          bindings=[] :: [proplists:property()],
          settings=#{} :: map()
         }).

-record(cms_asset, {
          id :: [string()],
          name :: binary(),
          description = <<>> :: binary(),
          file :: iodata(),
          minified=false :: boolean(),
          type :: asset_type(),
          settings=#{} :: map()
         }).

-record(cms_block_asset, {
          id :: {string(), string()},
          asset_id :: [string()],
          sort=1 :: integer(),
          settings=#{} :: map()
         }).

-record(cms_page, {
          id :: string(),
          module = index :: module(),
          accepted_role :: atom(),
          title = <<"LiquidCMS">> :: binary(),
          settings = #{}
         }).

-record(cms_user, {
          id :: binary(),
          email :: string(),
          password :: binary(),
          role :: role(),
          settings = #{} :: map()
         }).


-record(cms_account, {
          id :: any(),
          user :: binary(),
          settings = #{} :: map()
         }).
          
           


