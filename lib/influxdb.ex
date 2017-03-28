defmodule InfluxDB do
  @moduledoc """
  Main interface to query and insert data into InfluxDB.
  """

  alias InfluxDB.Config

  @type config :: Config.t
  @type time_unit :: :hour | :minute | :second | :millisecond | :microsecond | :nanosecond

  @doc """
  Send a query to InfluxDB and return the result.

  In case of success, it will return either:

    * `:ok`, when the query doesn't have any result set
    * `{:ok, [result]}`, where result is a list of series, and a series is a
    map containing the `:name`, `:columns`, `:rows` and `:tags` keys.

  In case of error, it will return either:

    * `{:error, {:not_found, description}}}`, when the series being queried could not be found
    * `{:error, {:server_error, description}}}`, when a server error occurs
  """
  @spec query(config, query, query_parameters, query_options) ::
      :ok
    | {:ok, [result]}
    | {:error, {:not_found, charlist}}
    | {:error, {:server_error, charlist}}

  @type query :: iodata
  @type query_parameters :: %{
    optional(atom) => atom | String.t | number
  }
  @type query_options :: %{
    optional(:timeout) => timeout,
    optional(:precision) => time_unit,
    optional(:retention_policy) => String.t
  }

  @type result :: [series]
  @type series :: %{
    required(:name) => String.t,
    required(:columns) => [String.t],
    required(:rows) => [tuple],
    optional(:tags) => %{
      optional(String.t) => String.t
    }
  }

  defdelegate query(config, query, params \\ %{}, options \\ %{}), to: :influxdb

  @doc """
  Write one or more points to InfluxDB.

  In case of success, it will return either:

    * `:ok`, when the write completes successfully

  In case of error, it will return either:

    * `{:error, {:not_found, description}}}`, when the database could not be found
    * `{:error, {:server_error, description}}}`, when a server error occurs
  """
  @spec write(config, points, write_options) ::
      :ok
    | {:error, {:not_found, charlist}}
    | {:error, {:server_error, charlist}}

  @type points :: [point]
  @type point :: {measurement, tags, fields, timestamp} | {measurement, tags, fields}
  @type measurement :: key
  @type tags :: %{ optional(key) => iodata | atom }
  @type fields :: %{ optional(key) => number | boolean | iodata | atom }
  @type key :: iodata | atom
  @type timestamp :: integer

  @type write_options :: %{
    optional(:timeout) => timeout,
    optional(:precision) => time_unit,
    optional(:retention_policy) => String.t
  }
  defdelegate write(config, points, options \\ %{}), to: :influxdb
end
