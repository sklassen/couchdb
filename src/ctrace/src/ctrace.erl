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

-module(ctrace).

-vsn(1).


-export([
    is_enabled/0,

    start_span/1,
    start_span/2,
    finish_span/0,
    finish_span/1,
    with_span/2,

    set_operation_name/1,
    add_tags/1,
    log/1,
    log/2,

    fun_to_op/1,

    get_operation_name/0,
    get_tags/0,
    get_refs/0,
    get_trace_id/0,
    get_span_id/0,
    get_tracer/0,
    get_context/0
]).


-include_lib("passage/include/opentracing.hrl").

-define(ENABLED_KEY, '$ctrace_enabled$').


-type tags() :: #{atom() => term()}.
-type log_fields() :: #{atom() => term()}.


-spec is_enabled() -> boolean().
is_enabled() ->
    case get(?ENABLED_KEY) of
        true -> true;
        false -> false;
        undefined ->
            Result = ctrace_config:is_enabled(),
            put(?ENABLED_KEY, Result),
            Result
    end.


-spec start_span(OperationName :: atom()) -> ok.

start_span(undefined) ->
    start_span(ctrace_config:default_tracer(), []);

start_span(OperationName) ->
    start_span(OperationName, []).


-spec start_span(OperationName :: atom(), Options :: [term()]) -> ok.

start_span(OperationName, Options0) ->
    case is_enabled() of
        true ->
            CurrSpan = passage_pd:current_span(),
            Options1 = case lists:keymember(tracer, 1, Options0) of
                true -> Options0;
                false -> [{tracer, jaeger_passage_reporter} | Options0]
            end,
            passage_pd:start_span(OperationName, Options1),
            if CurrSpan == undefined -> ok; true ->
                ParentOp = passage_span:get_operation_name(CurrSpan),
                passage_pd:set_tags(#{parent => ParentOp})
            end;
        false ->
            ok
    end.


-spec finish_span() -> ok.

finish_span() ->
    finish_span([]).


-spec finish_span(Options :: [term()]) -> ok.

finish_span(Options) ->
    case is_enabled() of
        true ->
            passage_pd:finish_span(Options);
        _ ->
            ok
    end.


%-spec with_span(OperationName :: atom(), Fun :: fun() -> term()) -> term().
with_span(OperationName, Fun) ->
    case is_enabled() of
        true ->
            try
                start_span(OperationName, []),
                Fun()
            catch Type:Reason ->
                Stack = erlang:get_stacktrace(),
                log(#{
                    ?LOG_FIELD_ERROR_KIND => Type,
                    ?LOG_FIELD_MESSAGE => Reason,
                    ?LOG_FIELD_STACK => Stack
                }, [error]),
                erlang:raise(Type, Reason, Stack)
            after
                finish_span()
            end;
        false ->
            Fun()
    end.


-spec set_operation_name(OperationName :: atom()) -> ok.

set_operation_name(OperationName) ->
    case is_enabled() of
        true ->
            passage_pd:set_operation_name(OperationName);
        _ ->
            ok
    end.


-spec add_tags(Tags :: tags()) -> ok.

add_tags(Tags) ->
    case is_enabled() of
        true ->
            passage_pd:set_tags(Tags);
        _ ->
            ok
    end.


-spec log(Fields :: log_fields()) -> ok.

log(FieldsOrFun) ->
    log(FieldsOrFun, []).


-spec log(Fields :: log_fields(), Options :: [term()]) -> ok.

log(FieldsOrFun, Options) ->
    case is_enabled() of
        true ->
            passage_pd:log(FieldsOrFun, Options);
        false ->
            ok
    end.


fun_to_op(Fun) ->
    {module, M} = erlang:fun_info(Fun, module),
    {name, F} = erlang:fun_info(Fun, name),
    {arity, A} = erlang:fun_info(Fun, arity),
    Str = io_lib:format("~s:~s/~b", [M, F, A]),
    list_to_atom(lists:flatten(Str)).


-spec get_tags() -> tags() | undefined.

get_tags() ->
    case is_enabled() of
        true ->
            passage_span:get_tags(passage_pd:current_span());
        false ->
            undefined
    end.


-spec get_refs() -> passage:refs() | undefined.

get_refs() ->
    case is_enabled() of
        true ->
            passage_span:get_refs(passage_pd:current_span());
        false ->
            undefined
    end.


-spec get_operation_name() -> atom().

get_operation_name() ->
    case is_enabled() of
        true ->
            passage_span:get_operation_name(passage_pd:current_span());
        false ->
            undefined
    end.


-spec get_trace_id() -> passage:trace_id() | undefined.

get_trace_id() ->
    case is_enabled() of
        true ->
            jaeger_passage_span_context:get_trace_id(get_context());
        false ->
            undefined
    end.


-spec get_span_id() -> passage:span_id() | undefined.
get_span_id() ->
    case is_enabled() of
        true ->
            jaeger_passage_span_context:get_span_id(get_context());
        false ->
            undefined
    end.


-spec get_tracer() -> passage:tracer_id().

get_tracer() ->
    case is_enabled() of
        true ->
            passage_span:get_tracer(passage_pd:current_span());
        false ->
            undefined
    end.


-spec get_context() -> passage_span_contest:context().

get_context() ->
    case is_enabled() of
        true ->
            Span = passage_pd:current_span(),
            passage_span:get_context(Span);
        false ->
            undefined
    end.
