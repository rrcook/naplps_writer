# Copyright 2026, Ralph Richard Cook
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

defmodule XYUtilities do
  @moduledoc """
  Encoding and decoding of NAPLPS multi-value X/Y coordinate data.

  NAPLPS stores drawing coordinates as packed bit fields rather than plain
  bytes. Each coordinate is a signed fixed-point *fraction* in the range
  `[-1.0, 1.0)`, and X and Y values are interleaved across a run of bytes: every
  byte begins with the flag bits `0b11` followed by three X bits and three Y
  bits. Three such bytes form one "chunk" carrying a 9-bit X value and a 9-bit Y
  value.

  This module provides the low-level helpers for that format (`build_bits/1`,
  `calculate/1`, the bit-list conversions) plus higher-level pipelines that go
  all the way from a hex string to usable coordinates (`text_to_xys/1`) and on to
  a TeliDraw command fragment (`array_to_telidraw/1`).

  Decoded fractions are scaled by 256 (`mul_256/1`) to move them into the 0–256
  device space the rest of the writer works in.
  """

  import Bitwise

  # Decodes one 3-byte NAPLPS coordinate chunk into its X and Y bit lists.
  #
  # Each of the three bytes carries the leading flag bits `0b11`, then three X
  # bits and three Y bits (most-significant first). Returns `{x_bits, y_bits}`,
  # where each element is a 9-element list of bits suitable for `calculate/1`.
  defp build_bits(bytes) do
    <<0b11::2, xbit9::1, xbit8::1, xbit7::1, ybit9::1, ybit8::1, ybit7::1, 0b11::2, xbit6::1,
      xbit5::1, xbit4::1, ybit6::1, ybit5::1, ybit4::1, 0b11::2, xbit3::1, xbit2::1, xbit1::1,
      ybit3::1, ybit2::1, ybit1::1>> = bytes

    {[xbit9, xbit8, xbit7, xbit6, xbit5, xbit4, xbit3, xbit2, xbit1],
     [ybit9, ybit8, ybit7, ybit6, ybit5, ybit4, ybit3, ybit2, ybit1]}
  end

  # Packs a list of bits (each `0` or `1`) into a binary, one bit per element.
  # Inverse of `byte_to_bitlist/1`.
  defp bitlist_to_byte(bitlist) do
    for b <- bitlist, into: <<>>, do: <<b::size(1)>>
  end

  # Expands a binary into a list of its individual bits.
  # Inverse of `bitlist_to_byte/1`.
  defp byte_to_bitlist(byte) do
    for <<bit::1 <- byte>>, do: bit
  end

  # Returns the two's-complement negation of a single-byte binary.
  defp twos(binary) do
    <<value>> = binary
    comp = bnot(value) + 1
    <<comp>>
  end

  @doc """
  Scales each `{x, y}` tuple in the list up by 256.

  Moves normalized fractional coordinates (`[-1.0, 1.0)`) into the 0–256 device
  space. Inverse of `div_256/1`.
  """
  def mul_256(xys), do: Enum.map(xys, fn {x, y} -> {x * 256, y * 256} end)

  @doc """
  Scales each `{x, y}` tuple in the list down by 256.

  Moves device-space coordinates back to normalized fractions. Inverse of
  `mul_256/1`.
  """
  def div_256(xys), do: Enum.map(xys, fn {x, y} -> {x / 256, y / 256} end)

  @doc """
  Interprets a 9-bit list as a signed fixed-point fraction in `[-1.0, 1.0)`.

  The leading bit is the sign. When it is `1` the remaining 8 bits are treated as
  two's-complement and the result is negated; otherwise they are summed directly.
  In both cases the bits are accumulated as binary fractional place values
  (½, ¼, ⅛, …).
  """
  def calculate(bitlist) do
    sign = hd(bitlist)

    cond do
      sign == 1 ->
        twos_bitlist = bitlist_to_byte(tl(bitlist)) |> twos |> byte_to_bitlist
        -1 * calc(twos_bitlist, 0.5, 0)

      true ->
        calc(tl(bitlist), 0.5, 0)
    end
  end

  # Accumulates a list of bits as a binary fraction, halving `multiplier` each
  # step. Starting `multiplier` is `0.5` and `acc` is `0`. Helper for
  # `calculate/1`.
  defp calc([], _multiplier, acc), do: acc

  defp calc([hd | tl], multiplier, acc), do: calc(tl, multiplier / 2, acc + hd * multiplier)

  @doc """
  Parses a whitespace-separated hex string into a binary.

  Strips spaces, upper-cases the input, and Base16-decodes it. Returns
  `{:ok, binary}` or `:error`.
  """
  def make_binary(text),
    do: String.replace(text, ~r/ */, "") |> String.upcase() |> Base.decode16()

  @doc """
  Splits a NAPLPS coordinate binary into 3-byte chunks and decodes each one.

  Returns a list of `{x, y}` fractional coordinate tuples in source order.
  """
  def dechunk(bytes), do: dechk(bytes, [])

  # Recursion worker behind `dechunk/1`: accumulates decoded coordinate tuples.
  defp dechk(<<>>, acc), do: Enum.reverse(acc)

  defp dechk(<<b1::8, b2::8, b3::8, rest::binary>>, acc) do
    chunk = <<b1, b2, b3>>
    {xs, ys} = build_bits(chunk)
    dechk(rest, [{calculate(xs), calculate(ys)} | acc])
  end

  @doc """
  Decodes a hex string of NAPLPS coordinate data into device-space coordinates.

  Convenience pipeline: `make_binary/1` → `dechunk/1` → `mul_256/1`, yielding a
  list of `{x, y}` tuples scaled into 0–256 space.
  """
  def text_to_xys(text) do
    {:ok, data} = make_binary(text)
    dechunk(data) |> mul_256
  end

  @doc """
  Renders a list of `{x, y}` coordinate tuples as a TeliDraw command fragment.

  Each value is formatted as an `n/256` fraction (rounded to the nearest
  integer), producing space-separated `"x/256 y/256 "` pairs.
  """
  def array_to_telidraw(xys) do
    for {x, y} <- xys, into: <<>>, do: "#{round(x)}/256 #{round(y)}/256 "
  end
end
