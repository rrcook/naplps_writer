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

defmodule NaplpsWriter do

  @moduledoc """
  Documentation for `NaplpsWriter`.
  """


  use NaplpsConstants

  # Many commands and operands are single bytes, no calculation
  # What is happening will be obvious from the byte passed in
  def append_byte(buffer, byte), do: buffer <> << byte >>

  def append_bytes(buffer, bytes), do: buffer <> IO.iodata_to_binary(bytes)

  defp prepend_sign(number, char_list) do
    cond do
      number < 0 -> [?1 | char_list]
      true -> [?0 | char_list]
    end
  end

  ##################
  # Functions to do conversion of x and y co-ordinates into the multi-byte format that
  # NAPLPS requires.
  # The entry is mb_xy, as a single value or a list
  #

  defp char_to_bit(?1), do: 1
  defp char_to_bit(_), do: 0

  defp mb_buildxy(buffer, xys) when is_list(xys) and length(xys) < 3 do
    buffer
  end

  # Convert, 3 x and y bits at a time, the 9 bits of binary fraction into
  # bytes. Each byte has two bits, 11, then three x bits then 3 y bits.
  defp mb_buildxy(buffer, xys) do
    [{xbit1, ybit1}, {xbit2, ybit2}, {xbit3, ybit3} | rest] = xys

    xybyte = <<0b11::2, xbit1::1, xbit2::1, xbit3::1, ybit1::1, ybit2::1, ybit3::1>>
    mb_buildxy(buffer <> xybyte, rest)
  end

  # Take a fraction, < 1, convert to a binary fraction as a string
  # according to the conversion function. If the fraction is less than
  # 0 then do a two's compliment on it and prepend a 1.
  # Turn from a string into a list of bits for further processing
  def make_bits(fraction) do
    bitfrac = FractionConverter.decimal_to_binary_fraction(fraction)

    bitstext = cond do
      fraction < 0 ->
        {bnum, _remainder} = Integer.parse(bitfrac, 2)
        bcomp = (bnot(bnum) + 1) &&& 0xff
        Integer.to_string(bcomp, 2)
      true ->
        bitfrac
    end

    Enum.map(prepend_sign(fraction, to_charlist(bitstext)), &char_to_bit/1)
  end

  def mb_xy(buffer, xys ) when is_list(xys) do
    pts_buffer = Enum.map(xys, fn xy -> mb_xy(<<>>, xy) end)

    # buffer <> Enum.reduce(pts_buffer, <<>>, &(&2 <> &1))
    buffer <> IO.iodata_to_binary(pts_buffer)
  end

  # Build two lists, for x and y, of 9 bits consisting of a sign bit
  # and a binary fraction of 8 bits. This will be stuffed into
  # three bytes according to the NAPLPS spec.
  def mb_xy(buffer, {x, y}) do
    x_frac = make_bits(x)
    y_frac = make_bits(y)

    # Break up the bits lists into a zipped list of tuples, then build the NAPLPS buffer
    buffer <> mb_buildxy(<<>>, Enum.zip(x_frac, y_frac))
  end

  def gcu_init(), do: gcu_init(<<>>)

  def gcu_init(buffer) do

    init_buffer = <<
      @cmd_domain,
      0xC8>>              # 2 dimensions on points, multivalue operands is 3 bytes,
                         # single value operand is one byte
      <>
      mb_xy(<<>>, {1 / 256, 1 / 256}) # pixel width/height of 1/1, in muli-byte format
      <>
      <<
      @cmd_texture_attr,
      0xC0,              # Solid Fill, don't draw outline of fills, solid line
      0xC0, 0xD2, 0xC0,
      @cmd_shift_in
    >>

    buffer <> init_buffer
  end

  def select_color(buffer, color) do
    buffer <> <<@cmd_select_color, color>>
  end

  def draw(buffer, command, points) when is_list(points) do
    buffer
    |> append_byte(command)
    |> mb_xy(points)

  end

  def draw(buffer, command, point) do
    draw(buffer, command, [ point ])
  end

  def draw_text_raw(buffer, text) do
    buffer <> text
  end

  def draw_text_abs(buffer, text, point), do: draw_text(buffer, @cmd_set_point_abs, text, point)

  def draw_text_rel(buffer, text, point), do: draw_text(buffer, @cmd_set_point_rel, text, point)

  defp draw_text(buffer, command, text, point) do
    buffer
    |> draw(command, point)
    |> draw_text_raw(text)
  end

  def text_attributes(buffer, point) do
    text_size_buffer = mb_xy(<<>>, point)
    # mvp at the moment - assume default rotation, cursor etc.
    # Proportional spacing, same as used in Prodigy
    default_text_buffer = <<@cmd_text_attr, 0xF0, 0xC0>>
    buffer <> default_text_buffer <> text_size_buffer
  end

end
