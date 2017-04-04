defmodule InfluxDB.Mixfile do
  use Mix.Project

  def project do
    [app: :influxdb,
     name: "InfluxDB",
     version: "0.2.1",
     elixir: "~> 1.0",
     package: package(),
     description: "InfluxDB client library",
     deps: deps()]
  end

  def application do
    [mod: {:influxdb_app, []},
     registered: [:influxdb_sup],
     applications: [:inets, :jsone]]
  end

  defp package() do
    [maintainers: ["Ali Sabil"],
     files: [
       "lib",
       "LICENSE*",
       "mix.exs",
       "README*",
       "rebar.config",
       "rebar.lock",
       "src"
     ],
     build_tools: ["mix", "rebar3"],
     licenses: ["Apache 2.0"],
     links: %{"Github" => "https://github.com/kopera/erlang-influxdb"}]
  end

  defp deps do
    [{:jsone, "~> 1.4.3"},
     {:ex_doc, "~> 0.15.0", only: :dev}]
  end
end
