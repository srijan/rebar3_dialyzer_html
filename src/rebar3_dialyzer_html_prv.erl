-module(rebar3_dialyzer_html_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, dialyzer_html).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 dialyzer_html"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Generate an HTML report from dialyzer output"},
            {desc, "Generate an HTML report from dialyzer output"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar_state:project_apps(State),
    AppDirs =
        lists:foreach(
          fun(App) ->
                  rebar_app_info:dir(App)
          end,
          Apps
         ),
    rebar_api:console("AppDirs = ~p", [AppDirs]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
