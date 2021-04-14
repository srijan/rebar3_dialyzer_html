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
    rebar_api:info("Generating Dialyzer HTML Report", []),
    DialyzerOutFileName = get_dialyzer_output_file(State),
    rebar_api:debug("Dialyzer output file name: ~p", [DialyzerOutFileName]),


    %% TODO: Break into functions

    {ok, Warnings} = file:consult(DialyzerOutFileName),

    ProjectDir = rebar_state:dir(State),
    ProjectName = filename:basename(ProjectDir),
    rebar_api:debug("Project Name: ~p", [ProjectName]),

    RebarOpts = rebar_state:opts(State),

    %% Build warnings dict, key = app name
    WarningsDict =
        lists:foldl(
          fun({_Tag, {Source, Line}, _Msg} = Warning, AccIn) ->

                  PlainMsg = dialyzer:format_warning(Warning),
                  FormattedMsg = rebar_dialyzer_format:format_warnings(RebarOpts, [Warning]),
                  FormattedMsgCleaned =
                      case re:split(FormattedMsg, "0m: ", [{return, list}]) of
                          [_IgnoredPrefix, ActualMessage] ->
                              ActualMessage;
                          _Else ->
                              FormattedMsg
                      end,

                  %% Inserts a newline after sentence ends
                  FormattedMsgSpaced = re:replace(FormattedMsgCleaned, "(\\.)([^a-zA-Z0-9_\\.\\]])", "\\1\n\\2", [global]),

                  AppName = get_appname_for_src(State, Source),
                  case string:prefix(Source, ProjectDir) of
                      nomatch ->
                          %% TODO: Some error outside the project ???
                          AccIn;
                      SourceRelPath ->
                          %% Update dict
                          dict:append(AppName,
                                      #{app_name => AppName,
                                        source => SourceRelPath,
                                        line => erlang:integer_to_list(Line),
                                        plain_message => PlainMsg,
                                        formatted_message => FormattedMsgSpaced
                                       },
                                      AccIn)
                  end
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
                                    "<div class=\"container\">"
                                    "<table><tr><th>File</th><th>Line</th><th>Warning</th></tr>"
                                   ],
                       AppOutput = lists:map(
                                     fun(Warning) ->
                                             [
                                              "<tr><td>",
                                              maps:get(source, Warning),
                                              "</td><td>",
                                              maps:get(line, Warning),
                                              "</td><td>",
                                              "<div class=\"warning\">",
                                              "<div class=\"warning-text\">",
                                              maps:get(plain_message, Warning),
                                              "</div>",
                                              "<div class=\"warning-hidden\">",
                                              maps:get(formatted_message, Warning),
                                              "</div></div>",
                                              "</td></tr>\n"
                                             ]
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
         "<html>\n<head>\n<title>Dialyzer Report for project: ", ProjectName, "</title>\n",
         "<script src=\"https://unpkg.com/ansi_up/ansi_up.js\" type=\"text/javascript\"></script>\n",
         "<style>\n",
         ".warning-hidden { display: none; }",
         ".warning-text { word-break: break-word; }"
         ".ansi-red-fg { background-color: #ff3434b3; }",
         ".ansi-green-fg { background-color: #97ea9799; }",
         ".ansi-blue-fg { background-color: #8AA9D5; }",
         ".ansi-cyan-fg { background-color: #9AD1D4; }",
         "table, th, td { border: 1px solid black; border-collapse: collapse; }\n",
         "th, td { padding: 15px; }\n",
         "table tr:nth-child(even) { background-color: #eee; }\n",
         "table tr:nth-child(odd) { background-color: #fff; }\n",
         "table th { color: white; background-color: #555; }\n"
         "</style>\n</head>\n<body>\n",
         "<h2>Dialyzer Report for project: ", ProjectName, "</h2>\n",
         "<h3>Total Warnings: ", erlang:integer_to_list(Count), "</h3>",
         "<p>Report generated at: ", GenTime, "</p><hr/>"
        ],
    Footer = [
              "<script>
                  var ansi_up = new AnsiUp;
                  ansi_up.use_classes = true;
                  ansi_up.escape_txt_for_html = function(txt) { return txt.replace(/\\n/gm, \"<br/>\").replace(/  /gm, \"&nbsp;&nbsp;\") };
                  var warnings = document.getElementsByClassName(\"warning-hidden\");
                  Array.from(warnings).forEach(
                    function(warning) {
                      warning.parentElement.getElementsByClassName(\"warning-text\")[0].innerHTML = ansi_up.ansi_to_html(warning.innerHTML);
                    }
                  )
               </script>",
              "</body></html>"
             ],

    %% Write file
    OutFile = "_build/default/dialyzer_report.html",
    ok = file:write_file(OutFile, [Header, Output, Footer]),

    {ok, State}.

%% -spec format_error(any()) ->  iolist().
%% format_error(Reason) ->
%%     io_lib:format("~p", [Reason]).

-spec format_error(any()) -> iolist().
%% format_error({error_processing_apps, Error}) ->
%%     io_lib:format("Error in dialyzing apps: ~ts", [Error]);
%% format_error({dialyzer_warnings, Warnings}) ->
%%     io_lib:format("Warnings occurred running dialyzer: ~b", [Warnings]);
%% format_error({unknown_application, App}) ->
%%     io_lib:format("Could not find application: ~ts", [App]);
%% format_error({unknown_module, Mod}) ->
%%     io_lib:format("Could not find module: ~ts", [Mod]);
%% format_error({duplicate_module, Mod, File1, File2}) ->
%%     io_lib:format("Duplicates of module ~ts: ~ts ~ts", [Mod, File1, File2]);
format_error({dialyzer_output_file_error, File, Error}) ->
    Error1 = file:format_error(Error),
    io_lib:format("Unable to read dialyzer output file ~ts: ~ts", [File, Error1]);
format_error({output_file_error, File, Error}) ->
    Error1 = file:format_error(Error),
    io_lib:format("Failed to write to ~ts: ~ts", [File, Error1]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal Functions

get_dialyzer_output_file(State) ->
    BaseDir = rebar_dir:base_dir(State),
    Output = filename:join(BaseDir, default_dialyzer_output_file()),
    case file:open(Output, [read]) of
        {ok, File} ->
            ok = file:close(File),
            Output;
        {error, Reason} ->
            throw({dialyzer_output_file_error, Output, Reason})
    end.

default_dialyzer_output_file() ->
    rebar_utils:otp_release() ++ ".dialyzer_warnings".


get_appname_for_src(State, Source) ->
    Apps = rebar_state:project_apps(State),
    case lists:filtermap(
                  fun(App) ->
                          AppDir = rebar_app_info:dir(App),
                          AppSrcDir = filename:join(AppDir, "src"),
                          case string:prefix(Source, AppSrcDir) of
                              nomatch -> false;
                              _Matched -> {true, rebar_app_info:name(App)}
                          end
                  end,
                  Apps
                 ) of
        [AppName] ->
            AppName;
        _Other ->
            "Unknown"
    end.
