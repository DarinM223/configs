defmodule History do
  def write(num \\ 100, path \\ "~/.ex_history.ex") do
    {:ok, :ok} = File.open(Path.expand(path), [:write, :utf8], fn file ->
      history =
        :lists.filter(&(not matches_history(&1)), :group_history.load)
        |> :lists.sublist(1, num)
      IO.write(file, :lists.reverse(history))
    end)
  end

  def clear(path \\ "~/.cache/erlang-history/") do
    {:ok, _} = File.rm_rf(Path.expand(path))
  end

  defp matches_history(s) do
    :string.equal(:string.slice(s, 0, 7), 'History')
  end
end
