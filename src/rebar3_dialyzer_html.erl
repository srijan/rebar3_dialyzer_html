-module(rebar3_dialyzer_html).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_dialyzer_html_prv:init(State),
    {ok, State1}.
