defmodule WhatAreTheOdds.RoomChannel do
  use Phoenix.Channel

  intercept ["new:guess"]

  def get_agent_name(code) do
    {:via, Registry, {Registry.BetState, code}}
  end

  def start_agent(code) do
    name = get_agent_name(code)
    {:ok, _} = Agent.start_link(fn -> %{} end, name: name)
    name
  end

  def join("bet:join:" <> code, %{"bet" => bet, "name" => name}, socket) do
    agent_name = start_agent(code)
    Agent.update(agent_name, fn _ -> %{bet: bet, name: name} end)
    new_socket = 
      socket
      |> assign(:agent_name, agent_name)
    {:ok, new_socket}
  end

  def join("bet:join:" <> code, _params, socket) do
    agent_name = get_agent_name(code)
    try do
      Agent.get(agent_name, fn %{bet: bet, name: name} ->
        {:ok, %{bet: bet, name: name}, socket}
      end)
    catch
      :exit, _ -> {:error, %{reason: "This bet does not exist"}}
    end
  end


  def handle_in("new:odds", %{"odds" => odds}, socket) do
    broadcast! socket, "new:odds", %{odds: odds}
    {:noreply, socket}
  end

  def handle_in("received:odds", _params, socket) do
    broadcast! socket, "received:odds", %{}
    {:noreply, socket}
  end

  def handle_in("new:guess", %{"guess" => guess}, socket) do
    new_socket = assign(socket, :ignore, true)
    broadcast! new_socket, "new:guess", %{guess: guess}
    {:noreply, new_socket}
  end

  def handle_out("new:guess", payload, socket) do
    if !socket.assigns[:ignore] do
      push socket, "new:guess", payload
    end
    new_socket = assign(socket, :ignore, false)
    {:noreply, new_socket}
  end
end
