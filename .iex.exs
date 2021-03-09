defmodule History do
  def write(num \\ 100, path \\ "~/.ex_history.ex") do
    {:ok, :ok} = File.open(Path.expand(path), [:write, :utf8], fn file ->
      history =
        :group_history.load
        |> Stream.filter(&(not matches_history(&1)))
        |> Stream.take(num)
        |> Enum.reverse
      IO.write(file, history)
    end)
  end

  def clear(path \\ "~/.cache/erlang-history/") do
    # Use `:disk_log.accessible_logs` to find out where
    # the group_history file is.
    :disk_log.truncate(:'$#group_history')
    {:ok, _} = File.rm_rf(Path.expand(path))
  end

  defp matches_history(s) do
    :string.equal(:string.slice(s, 0, 7), 'History')
  end
end
