% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(ctrace_config).
-behaviour(config_listener).
-vsn(1).

-export([
    default_tracer/0,
    is_enabled/0,
    update/0
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).


-define(MAIN_TRACER, jaeger_passage_reporter).


-spec default_tracer() -> atom().

default_tracer() ->
    ?MAIN_TRACER.


-spec is_enabled() -> boolean().

is_enabled() ->
    config:get_boolean("tracing", "enabled", false).


-spec update() -> ok.

update() ->
    case is_enabled() of
        true ->
            maybe_start_main_tracer(?MAIN_TRACER) andalso update_config();
        false ->
            jaeger_passage:stop_tracer(?MAIN_TRACER)
    end,
    ok.


handle_config_change("tracing.samplers", OperationIdStr, Value, _, St) ->
    case is_enabled() of
        true -> update_sampler(OperationIdStr, Value);
        false -> ok
    end,
    {ok, St};
handle_config_change("tracing." ++ OperationIdStr, _Key, _Val, _Persist, St) ->
    case is_enabled() of
        true -> update_sampler(OperationIdStr);
        false -> ok
    end,
    {ok, St};
handle_config_change("tracing", "enabled", _, _Persist, St) ->
    update(),
    {ok, St};
handle_config_change(_Sec, _Key, _Val, _Persist, St) ->
    {ok, St}.


handle_config_terminate(_Server, _Reason, _State) ->
    update().


maybe_start_main_tracer(TracerId) ->
    case passage_tracer_registry:get_reporter(TracerId) of
        error ->
            start_main_tracer(TracerId);
        _ ->
            true
    end.


start_main_tracer(TracerId) ->
    Format = list_to_atom(config:get("tracing", "thrift_format", "compact")),
    Host = config:get("tracing", "agent_host", "127.0.0.1"),
    Port = config:get_integer("tracing", "agent_port", 6831),
    Name = list_to_atom(config:get("tracing", "app_name", "couchdb")),

    Sampler = passage_sampler_all:new(),
    Options = [
        {thrift_format, Format},
        {agent_host, Host},
        {agent_port, Port},
        {default_service_name, Name}
    ],

    case jaeger_passage:start_tracer(TracerId, Sampler, Options) of
        ok ->
            true;
        {error, Reason} ->
            couch_log:error("Cannot start main tracer: ~p~n", [Reason]),
            false
    end.


update_config() ->
    lists:foreach(fun({OperationIdStr, SamplerDef}) ->
        update_sampler(OperationIdStr, SamplerDef)
    end, config:get("tracing.samplers")).


update_sampler(OperationIdStr) when is_list(OperationIdStr) ->
    case config:get("tracing.samplers", OperationIdStr) of
        undefined ->
            rem_tracer(OperationIdStr);
        SamplerDef ->
            update_sampler(OperationIdStr, SamplerDef)
    end.


update_sampler(OperationIdStr, deleted) ->
    rem_tracer(OperationIdStr);

update_sampler(OperationIdStr, SamplerDef) ->
    case parse_sampler(SamplerDef) of
        undefined ->
            rem_tracer(OperationIdStr);
        Sampler ->
            compile_rules(OperationIdStr),
            add_tracer(OperationIdStr, Sampler)
    end.


add_tracer(OperationIdStr, Sampler) ->
    OperationId = list_to_atom(OperationIdStr),
    case passage_tracer_registry:get_reporter(OperationId) of
        {ok, _} ->
            % Only need to update the sampler here as the
            % ctrace_filter will automatically update to use
            % the recompiled dynamic module.
            passage_tracer_registry:set_sampler(OperationId, Sampler);
        error ->
            Mod = filter_module_name(OperationIdStr),
            CTraceFilter = ctrace_filter:new(OperationId, Mod),
            Filter = passage_reporter:new(ctrace_filter, CTraceFilter),
            Ctx = jaeger_passage_span_context,
            passage_tracer_registry:register(OperationId, Ctx, Sampler, Filter)
    end.


rem_tracer(OperationIdStr) ->
    OperationId = list_to_atom(OperationIdStr),
    passage_tracer_registry:deregister(OperationId).


compile_rules(OperationIdStr) ->
    OperationId = list_to_atom(OperationIdStr),
    FilterMod = filter_module_name(OperationIdStr),
    RulesRaw = config:get("tracing." ++ OperationIdStr),
    try
        Rules = lists:map(fun({Name, RuleDef}) ->
            Rule = ctrace_dsl:parse_rule(Name, RuleDef),
            RawActions = maps:get(actions, Rule),
            Actions = lists:map(fun set_action/1, RawActions),
            maps:put(actions, Actions, Rule)
        end, RulesRaw),
        ctrace_dsl:compile(FilterMod, Rules)
    catch throw:{error, Reason} ->
        rem_tracer(OperationIdStr),
        couch_log:error("cannot compile '~s': ~p~n", [OperationId, Reason])
    end.


parse_sampler(Binary) when is_binary(Binary) ->
    parse_sampler(binary_to_list(Binary));
parse_sampler("all") ->
    ctrace_sampler:all();
parse_sampler("null") ->
    ctrace_sampler:null();
parse_sampler(FloatStr) ->
    Help = "Cannot parse sampler. The only supported formats are: "
        " all | null | float(), got '~s'",
    try
        ctrace_sampler:probalistic(binary_to_float(FloatStr))
    catch _:_ ->
        couch_log:error(Help, [FloatStr]),
        undefined
    end.


set_action({sample, Rate}) ->
    {ctrace_action, sample, [Rate]};
set_action(report) ->
    {ctrace_action, report, [?MAIN_TRACER]}.


filter_module_name(OperationIdStr) ->
    list_to_atom("ctrace_filter_" ++ OperationIdStr).
