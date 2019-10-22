defmodule Couch.CTrace.Test do
  require Logger
  use ExUnit.Case
  @moduletag capture_log: true

  setup do
    apps = :test_util.start_applications([:ctrace])
    :meck.new(:couch_log, [{:stub_all, :meck.val(:ok)}])
    :meck.new(:jaeger_passage_reporter, [:passthrough])
    :meck.expect(:jaeger_passage_reporter, :report, fn _, _ -> :ok end)

    on_exit(fn ->
      :test_util.stop_applications(apps)
      :meck.unload()
    end)

    :config.set('tracing.samplers', 'all-docs', 'all', false)
    :config.set('tracing.all-docs', 'all', ~C"(#{}) -> [report]", false)
    :config.set_boolean('tracing', 'enabled', true, false)

    {:ok, reporter} =
      :test_util.wait_other_value(
        fn ->
          :passage_tracer_registry.get_reporter(:"all-docs")
        end,
        :error
      )

    filter = :passage_reporter.get_state(reporter)
    %{filter: :ctrace_filter.module(filter)}
  end

  describe "Basic : " do
    test "spans are reported" do
      :ctrace.start_span(:"all-docs")
      :ctrace.finish_span()

      assert length(reports()) == 1
    end

    test "child spans are reported" do
      :ctrace.start_span(:"all-docs")
      :ctrace.start_span(:"child-span")
      :ctrace.finish_span()
      :ctrace.finish_span()

      assert length(reports()) == 2
    end
  end

  defp reports() do
    events =
      :meck.history(:jaeger_passage_reporter)
      |> Enum.filter(fn event ->
        {_, fun, _} = elem(event, 1)
        fun == :report
      end)

    events
    |> Enum.flat_map(fn event ->
      {_, _, args} = elem(event, 1)
      [args]
    end)
  end

  defp capture_logs(level, regexp) do
    history(:couch_log, level, 2)
    |> Enum.flat_map(fn event ->
      {_, _, [msg, args]} = elem(event, 1)

      if Regex.match?(regexp, List.to_string(msg)) do
        [args]
      else
        []
      end
    end)
  end

  defp history(module, function, arity) do
    history = :meck.history(module)

    history
    |> Enum.filter(fn event ->
      {_, fun, args} = elem(event, 1)
      function == fun and arity == length(args)
    end)
  end
end
