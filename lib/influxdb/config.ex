defmodule InfluxDB.Config do
  @moduledoc """
  All the InfluxDB api functions require a config data structure, this module
  provides the necessary functions for creating the config data structure.
  """

  @type t :: :influxdb_config.config

  @doc """
  Create a new config given the provided options

  ## Options

    * `:host` - the host to connect to. (default: localhost)
    * `:port` - the port to connect to. (default: 8086)
    * `:username` - the username to use for authentication. (default: root)
    * `:password` - the password to use for authentication. (default: root)
    * `:database` - the database to use.

  """
  @spec new(options) :: :influxdb_config.config
  @typep options :: %{
    optional(:host) => String.t,
    optional(:port) => 1..65535,
    optional(:username) => String.t,
    optional(:password) => String.t,
    optional(:database) => String.t
  }
  defdelegate new(opts), to: :influxdb_config

end
