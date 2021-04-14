rebar3_dialyzer_html
=====

A rebar plugin to generate HTML reported from dialyzer raw results

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_dialyzer_html, {git, "https://github.com/srijan/rebar3_dialyzer_html.git", {branch, "main"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 dialyzer_html
    ===> Fetching rebar3_dialyzer_html
    ===> Compiling rebar3_dialyzer_html
    <Plugin Output>
