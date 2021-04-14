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

    %% TODO: Get dialyzer output file name instead of hardcode
    RawFilename = "_build/default/23.3.1.dialyzer_warnings",

    %% Read dialyzer output file
    {ok, Warnings} = file:consult(RawFilename),

    %% Get apps and their source directory paths
    Apps = rebar_state:project_apps(State),
    AppDirs =
        lists:map(
          fun(App) ->
                  rebar_app_info:dir(App)
          end,
          Apps
         ),
    rebar_api:console("AppDirs = ~p", [AppDirs]),

    %% Build warnings dict, key = app name
    WarningsDict =
        lists:foldl(
          fun({_Tag, {Source, Line}, _Msg} = Warning, AccIn) ->
                  %% TODO: Format the warning
                  Text = dialyzer:format_warning(Warning),

                  %% TODO: Calculate the source file path and appname
                  AppName = "default",

                  %% Update dict
                  dict:append(AppName,
                              #{app_name => AppName,
                                source => Source,
                                line => Line,
                                message => Text},
                              AccIn)
          end,
          dict:new(),
          Warnings
         ),

    %% Build the output HTML
    Output = dict:fold(
               fun(AppName, AppWarnings, AccIn) ->
                       Count = length(AppWarnings),
                       AppHeader = [
                                    "<h3>App Name: ", AppName, "</h3>",
                                    "<h4>Number of Warnings for this app: ",
                                    erlang:integer_to_list(Count), "</h3>",
                                    "<div class=\"container\"><table>",
                                    "<tr><th>File</th><th>Line</th><th>Warning</th></tr>"
                                   ],
                       AppOutput = lists:map(
                                     fun(Warning) ->
                                             ["<tr><td>",
                                              maps:get(source, Warning),
                                              "</td><td>",
                                              maps:get(line, Warning),
                                              "</td><td>",
                                              maps:get(message, Warning),
                                              "</td></tr>\n"]
                                     end,
                                     AppWarnings
                                    ),
                       AppFooter = "</table></div><hr/>",
                       [AccIn, AppHeader, AppOutput, AppFooter]
               end,
               [],
               WarningsDict
              ),

    %% Add header/footer
    Count = length(Warnings),
    GenTime = httpd_util:rfc1123_date(),
    Header =
        [
         "<html>\n<head>\n<title>Dialyzer Report</title>\n<style>\n",
         "table, th, td { border: 1px solid black; border-collapse: collapse; }\n",
         "th, td { padding: 15px; }\n",
         "table tr:nth-child(even) { background-color: #eee; }\n",
         "table tr:nth-child(odd) { background-color: #fff; }\n",
         "table th { color: white; background-color: #555; }\n"
         "</style>\n</head>\n<body>\n",
         "<h2>Dialyzer Report</h2>\n",
         "<h3>Total Number of Warnings: ", erlang:integer_to_list(Count), "</h3>",
         "<h4>Report generated at: ", GenTime, "</h4><hr/>"
        ],
    Footer = "</body></html>",

    %% Write file
    OutFile = "_build/default/dialyzer_report.html",
    ok = file:write_file(OutFile, [Header, Output, Footer]),

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
