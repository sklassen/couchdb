#!/usr/bin/env escript
%%! -env ERL_LIBS src -noshell -s escript start

-mode(compile).

-export([main/1]).

-define(ANCESTORS_KEY, passage_span_ancestors).
-define(SPAN_TYPES, [
    <<"TransactionTrace_Commit">>,
    <<"TransactionTrace_Get">>,
    <<"TransactionTrace_GetRange">>,
    <<"TransactionTrace_GetVersion">>
]).
-define(TAG_KEYS, [
    {<<"Key">>, key},
    {<<"StartKey">>, 'start-key'},
    {<<"EndKey">>, 'end-key'},
    {<<"ValueSizeBytes">>, 'value-size-bytes'},
    {<<"RangeSizeBytes">>, 'range-size-bytes'},
    {<<"NumMutations">>, 'num-mutations'},
    {<<"CommitSizeBytes">>, 'commit-size-bytes'}
]).


main(Args) ->
    start_apps(),
    start_tracer(),

    lists:foreach(fun(FileName) ->
        load_file(FileName)
    end, Args),
    timer:sleep(1000).


load_file(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Lines = binary:split(Data, <<"\r\n">>, [global]),
    lists:foreach(fun(Line) ->
        case string:trim(Line) of
            <<>> ->
                ok;
            Else ->
                {Props} = jiffy:decode(Else),
                maybe_create_span(Props)
        end
    end, Lines).


maybe_create_span(Props) ->
    {_, Type} = lists:keyfind(<<"Type">>, 1, Props),
    case lists:member(Type, ?SPAN_TYPES) of
        true ->
            create_span(Type, Props);
        false ->
            ok
    end.


create_span(Type, Props) ->
    %io:format(standard_error, "~p~n", [Props]),
    {StartTime, EndTime} = get_time(Props),
    {TraceId, SpanId} = get_trace_ids(Props),
    Tags = get_tags(Props),

    fake_root(TraceId, SpanId),
    passage_pd:start_span(binary_to_atom(Type, utf8), [
        {time, StartTime},
        {tags, Tags}
    ]),
    passage_pd:finish_span([{time, EndTime}]).


get_time(Props) ->
    {_, EndTimeBin} = lists:keyfind(<<"Time">>, 1, Props),
    {_, LatencyBin} = lists:keyfind(<<"Latency">>, 1, Props),
    EndTimeFloat = binary_to_float(EndTimeBin),
    Latency = binary_to_float(LatencyBin),
    {float_to_time(EndTimeFloat - Latency), float_to_time(EndTimeFloat)}.


float_to_time(Val) ->
    Mega = trunc(Val / 1000000),
    Secs = trunc(Val) rem 1000000,
    Micro = trunc(Val * 1000000) rem 1000000,
    {Mega, Secs, Micro}.


get_trace_ids(Props) ->
    {_, TxId} = lists:keyfind(<<"TransactionID">>, 1, Props),
    [TraceIdBin, SpanIdBin] = binary:split(TxId, <<":">>),
    TraceId = mochihex:to_bin(binary_to_list(TraceIdBin)),
    SpanId = mochihex:to_bin(binary_to_list(SpanIdBin)),
    {
        binary:decode_unsigned(TraceId, little),
        binary:decode_unsigned(SpanId, little)
    }.


get_tags(Props) ->
    lists:foldl(fun({BinKey, AtomKey}, Tags) ->
        case lists:keyfind(BinKey, 1, Props) of
            {_, Value} ->
                Tags#{AtomKey => Value};
            false ->
                Tags
        end
    end, #{}, ?TAG_KEYS).


start_apps() ->
    Apps = [
        jiffy,
        passage,
        jaeger_passage
    ],
    lists:foreach(fun(App) ->
        {ok, _} = application:ensure_all_started(App)
    end, Apps).


start_tracer() ->
    Sampler = passage_sampler_all:new(),
    Options = [
        {thrift_format, compact},
        {agent_host, "127.0.0.1"},
        {agent_port, 6831},
        {default_service_name, 'fdb-client'}
    ],
    ok = jaeger_passage:start_tracer(main, Sampler, Options).


fake_root(TraceId, SpanId) ->
    Root = [{child_of, {
        passage_span,
        main,
        foo,
        {0, 0, 0},
        undefined,
        [],
        #{},
        [],
        {
            passage_span_context,
            {
                jaeger_passage_span_context,
                TraceId,
                SpanId,
                true,
                undefined
            },
            #{}
        }
    }}],
    put(?ANCESTORS_KEY, Root).