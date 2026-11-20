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
  def append_byte(buffer, byte), do: buffer <> <<byte>>

  def append_bytes(buffer, bytes), do: buffer <> IO.iodata_to_binary(bytes)

  ##################
  # Functions to do conversion of x and y co-ordinates into the multi-byte format that
  # NAPLPS requires.
  # The entry is mb_xy, as a single value or a list
  #

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

  # Take a fraction in (-1, 1) and return a 9-bit signed binary fraction
  # (1 sign bit + 8 fraction bits, two's complement) as a list of bits for
  # mb_buildxy, in units of 1/256.
  #
  # Coordinates are delta-encoded, so the quantization error of every point
  # accumulates along a polygon. Use round (not truncate) so the per-point
  # bias is centered on zero - truncating toward zero drifted large rings
  # by tens of pixels (e.g. the Southeast rain polygon crept ~20px north).
  # Building the value directly as a 9-bit two's complement also avoids the
  # old string-based pitfalls: dropped leading zeros for magnitudes > 0.5,
  # and a tiny negative encoding as 1_0000_0000 (= -1.0, a full-screen jump).
  def make_bits(fraction) do
    n = round(fraction * 256) |> max(-256) |> min(255)
    ninebit = n &&& 0x1FF
    for i <- 8..0//-1, do: ninebit >>> i &&& 1
  end

  def mb_xy(buffer, xys) when is_list(xys) do
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
    # single value operand is one byte
    # pixel width/height of 1/1, in muli-byte format
    init_buffer =
      <<
        @cmd_domain,
        # 2 dimensions on points, multivalue operands is 3 bytes,
        0xC8
      >> <>
        mb_xy(<<>>, {1 / 256, 1 / 256}) <>
        <<
          @cmd_texture_attr,
          # Solid Fill, don't draw outline of fills, solid line
          0xC0,
          0xC0,
          0xD2,
          0xC0,
          @cmd_shift_in
        >>

    buffer <> init_buffer
  end

  def select_color(buffer, color) do
    buffer <> <<@cmd_select_color, color>>
  end

  @doc """
  Append a DEF TEXTURE mask definition: `84 <mask> <body> 85` followed by GCU's code-set
  restore escape (`ESC ) W ESC ~`). `mask` is the mask byte `0x41..0x44` (A-D); `body` is the
  raw mask-definition bytes, emitted verbatim (GCU does not decode the body). Geometry for the
  body is authored with `draw/3` / `mb_xy/2` and passed in here. An operand outside A-D defines
  nothing in GCU; this function requires a valid mask byte (the Telidraw compiler validates the
  semantic 1-4 form and diagnoses anything else).

      iex> NaplpsWriter.def_texture(<<>>, 0x41, <<0xA4, 0xC0, 0xC0, 0xC9>>)
      <<0x84, 0x41, 0xA4, 0xC0, 0xC0, 0xC9, 0x85, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
  """
  def def_texture(buffer, mask, body) when mask in 0x41..0x44 and is_binary(body) do
    buffer <> <<@cmd_def_texture, mask>> <> body <> <<@cmd_end>> <> @esc_codeset_restore
  end

  def draw(buffer, command, points) when is_list(points) do
    buffer
    |> append_byte(command)
    |> mb_xy(points)
  end

  def draw(buffer, command, point) do
    draw(buffer, command, [point])
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
