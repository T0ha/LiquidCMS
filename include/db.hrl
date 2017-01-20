-define(DB_PREFIX(), liquid_cms_).
-type role() :: 'admin' | 'editor' | undefined.
%% DB tables records goes here
-record(cms_mfa, {
          id :: {string(), string()},
          sort :: non_neg_integer(),
          mfa :: {module(), fun(), [any()]},
          settings :: map()
         }).

-record(cms_template, {
          id :: binary(),
          path :: iodata(),
          page :: string(), 
          settings = #{}
         }).

-record(cms_page, {
          id :: string(),
          path :: iodata(),
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
          
           


