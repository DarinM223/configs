defmodule History do
  @default_history_size 100
  @history_path "~/.ex_history.ex"

  def write(num \\ @default_history_size, path \\ @history_path) do
    {:ok, :ok} =
      File.open(Path.expand(path), [:write, :utf8], fn file ->
        write_history(file, num)
      end)
  end

  def append(num \\ @default_history_size, path \\ @history_path) do
    {:ok, :ok} =
      File.open(Path.expand(path), [:append, :utf8], fn file ->
        IO.write(file, '\n')
        write_history(file, num)
      end)
  end

  defp write_history(file, num) do
    history =
      :group_history.load()
      |> Stream.filter(&(not matches_history(&1)))
      |> Stream.take(num)
      |> Enum.reverse()

    IO.write(file, history)
  end

  defp matches_history(s) do
    :string.equal(:string.slice(s, 0, 7), 'History')
  end

  def clear() do
    # Use `:disk_log.accessible_logs` to find out where
    # the group_history file is.
    :disk_log.truncate(:"$#group_history")
  end
end
