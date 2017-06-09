defmodule WhatAreTheOdds.PageController do
  use WhatAreTheOdds.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
