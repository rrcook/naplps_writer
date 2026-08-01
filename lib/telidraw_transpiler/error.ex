defmodule TelidrawTranspiler.Error do
  @moduledoc "Raised by `TelidrawTranspiler.transpile!/2` when compilation fails."

  defexception [:diagnostics, :message]

  @impl true
  def exception(diagnostics) when is_list(diagnostics) do
    msg =
      diagnostics
      |> Enum.filter(&(&1.severity == :error))
      |> Enum.map_join("\n", fn d -> "  #{d.line}:#{d.column} #{d.message}" end)

    %__MODULE__{diagnostics: diagnostics, message: "Telidraw compilation failed:\n" <> msg}
  end
end
