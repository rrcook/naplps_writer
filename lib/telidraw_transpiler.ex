# Copyright 2024, Ralph Richard Cook
#
# This file is part of Prodigy Reloaded.
#
# Prodigy Reloaded is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General
# Public License as published by the Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
#
# Prodigy Reloaded is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License along with Prodigy Reloaded. If not,
# see <https://www.gnu.org/licenses/>.

defmodule TelidrawTranspiler do
  @moduledoc """
  A transpiler for the **Telidraw** text-graphics language (`.td` files) that emits
  NAPLPS (`.nap`) byte streams using the primitives in `NaplpsWriter`.

  Telidraw is the human-editable source format from the
  [FoxCouncil/NAPLPS](https://github.com/FoxCouncil/NAPLPS) project. This module
  is an Elixir port of that project's `NAPLPS.Telidraw` namespace
  (Lexer → Parser → Compiler), retargeted so that every drawing command flows
  through `NaplpsWriter` rather than the C# `NaplpsCommandBuilder`.

  ## Usage

      {:ok, bytes} = TelidrawTranspiler.transpile(\"""
      #coord fractions
      with color 4 {
        move 0.25 0.1
        rect 0.5 0.35
      }
      \""")

      :ok = TelidrawTranspiler.transpile_file("house.td", "house.nap")

  ## Options

    * `:init` (default `true`) — prepend `NaplpsWriter.gcu_init/0`, which sets up
      the graphics environment (domain, texture, shift-in) so the resulting stream
      is a self-contained, drawable NAPLPS picture.

  ## Coverage

  All Telidraw drawing verbs, attributes, `with`/`repeat`/`for`/`if` blocks,
  `proc` definitions/calls (inlined), `let`/`palette` bindings, compile-time
  arithmetic, and the `raw` escape hatch are supported. Geometry is emitted at the
  default multi-byte value of 3 (three operand bytes per vertex), matching
  `NaplpsWriter.mb_xy/2` and `NaplpsWriter.gcu_init/0`.
  """

  alias TelidrawTranspiler.{Lexer, Parser, Compiler}

  @doc """
  Transpile Telidraw `source` into a NAPLPS byte binary.

  Returns `{:ok, binary}` on success, or `{:error, diagnostics}` where
  `diagnostics` is a list of `%{severity, line, column, message}` maps.
  """
  @spec transpile(String.t(), keyword()) :: {:ok, binary()} | {:error, [map()]}
  def transpile(source, opts \\ []) when is_binary(source) do
    {tokens, lex_diags} = Lexer.tokenize(source)

    case Parser.parse(tokens) do
      {:ok, program, parse_diags} ->
        {bytes, compile_diags} = Compiler.compile(program, opts)
        diags = lex_diags ++ parse_diags ++ compile_diags

        if Enum.any?(diags, &(&1.severity == :error)) do
          {:error, diags}
        else
          {:ok, bytes}
        end

      {:error, parse_diags} ->
        {:error, lex_diags ++ parse_diags}
    end
  end

  @doc """
  Transpile the Telidraw file at `input_path`, writing NAPLPS bytes to `output_path`.

  Returns `:ok`, or `{:error, reason}`.
  """
  @spec transpile_file(Path.t(), Path.t(), keyword()) :: :ok | {:error, term()}
  def transpile_file(input_path, output_path, opts \\ []) do
    with {:ok, source} <- File.read(input_path),
         {:ok, bytes} <- transpile(source, opts) do
      File.write(output_path, bytes)
    end
  end

  @doc """
  Like `transpile/2` but raises `TelidrawTranspiler.Error` on failure and returns
  the byte binary directly. Convenient in scripts and tests.
  """
  @spec transpile!(String.t(), keyword()) :: binary()
  def transpile!(source, opts \\ []) when is_binary(source) do
    case transpile(source, opts) do
      {:ok, bytes} -> bytes
      {:error, diags} -> raise TelidrawTranspiler.Error, diags
    end
  end
end
