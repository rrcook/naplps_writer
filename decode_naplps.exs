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

# Coordinate decoding lives in XYUtilities (lib/xy_utilities.ex), which is
# unit-tested. Load it from source when running standalone (`elixir
# decode_naplps.exs ...`); under `mix run` it is already compiled and on the
# code path, so skip the reload.
unless Code.ensure_loaded?(XYUtilities) do
  Code.require_file("lib/xy_utilities.ex", __DIR__)
end

defmodule NAPLPSDecoder do
  @moduledoc """
  NAPLPS Decoder - Analyzes NAPLPS drawing files and lists commands
  Based on the NAPLPS specification from NAP.txt

  Decodes an 8-bit NAPLPS stream into one listing entry per command:

    * PDI commands (`A0`-`BF`) with their operands: coordinates, the RESET flags, the DOMAIN, TEXT
      and TEXTURE fields, SELECT COLOR modes, SET COLOR groups, WAIT and BLINK intervals.
    * C1 controls (`80`-`9F`): DEF MACRO / DEFP / DEFT / DEF DRCS / DEF TEXTURE definitions up to
      their END (the body is listed indented), REPEAT, reverse video, underline, word wrap, scroll,
      the text sizes and the cursor controls.
    * C0 controls: SI / SO, SS2 / SS3, cursor controls, CURSOR POSITION, NSR.
    * Escape sequences: the G-set designations (`ESC ) W`, `ESC / z`, `ESC / }`, `ESC / {`, ...) and
      the locking shifts (`ESC n`, `ESC o`, `ESC ~`, ...). The decoder tracks which set is in GL and
      GR, so printable bytes are listed by the set they come from: TEXT (primary), SUPPLEMENTARY,
      MOSAIC, DRCS, or MACRO CALL when the macro set is in GL (`ESC / z ESC o <code>`) or is
      single-shifted (`ESC / z SS3 <code>`).

  These are the sequences NaplpsWriter and the Telidraw compiler write, including GCU's own forms:
  the code-set restore `ESC ) W ESC ~` after a definition or a macro call, `86 <count>` text runs,
  `BD DC <time>` waits, and the `A5` that ends every GCU file (listed as a POINT SET REL with no
  data bytes). As in GCU, the decoder state (GL, DOMAIN) carries through a definition body.

  Coordinates are listed in device units (0-256), 3-byte multi-value only (XYUtilities.dechunk/1).
  """

  import Bitwise

  # PDI Command names
  @pdi_commands %{
    0x86 => "REPEAT",
    0xA0 => "RESET",
    0xA1 => "DOMAIN",
    0xA2 => "TEXT ATTRIBUTES",
    0xA3 => "TEXTURE",
    0xA4 => "POINT SET ABS",
    0xA5 => "POINT SET REL",
    0xA6 => "POINT ABS",
    0xA7 => "POINT REL",
    0xA8 => "LINE ABS",
    0xA9 => "LINE REL",
    0xAA => "SET & LINE ABS",
    0xAB => "SET & LINE REL",
    0xAC => "ARC OUTLINED",
    0xAD => "ARC FILLED",
    0xAE => "SET & ARC OUTLINED",
    0xAF => "SET & ARC FILLED",
    0xB0 => "RECT OUTLINED",
    0xB1 => "RECT FILLED",
    0xB2 => "SET & RECT OUTLINED",
    0xB3 => "SET & RECT FILLED",
    0xB4 => "POLY OUTLINED",
    0xB5 => "POLY FILLED",
    0xB6 => "SET & POLY OUTLINED",
    0xB7 => "SET & POLY FILLED",
    0xB8 => "FIELD",
    0xB9 => "INCREMENTAL POINT",
    0xBA => "INCREMENTAL LINE",
    0xBB => "INCREMENTAL POLY FILLED",
    0xBC => "SET COLOR",
    0xBD => "WAIT",
    0xBE => "SELECT COLOR",
    0xBF => "BLINK"
  }

  # C1 controls other than the definitions (80-85) and REPEAT (86), which are decoded with their
  # operand bytes. NAP.txt "C1 Control Set".
  @c1_controls %{
    0x87 => {"REPEAT TO EOL", "repeat the previous character to the end of the line"},
    0x88 => {"REVERSE VIDEO", "on"},
    0x89 => {"NORMAL VIDEO", "reverse video off"},
    0x8A => {"SMALL TEXT", "character field 1/80 x 5/128"},
    0x8B => {"MEDIUM TEXT", "character field 1/32 x 3/64"},
    0x8C => {"NORMAL TEXT", "character field 1/40 x 5/128"},
    0x8D => {"DOUBLE HEIGHT", "character field 1/40 x 5/64"},
    0x8E => {"BLINK START", "blink the drawing color"},
    0x8F => {"DOUBLE SIZE", "character field 1/20 x 5/64"},
    0x90 => {"PROTECT", "protect the active field"},
    0x91 => {"EDC1", "reserved (discarded)"},
    0x92 => {"EDC2", "reserved (discarded)"},
    0x93 => {"EDC3", "reserved (discarded)"},
    0x94 => {"EDC4", "reserved (discarded)"},
    0x95 => {"WORD WRAP ON", ""},
    0x96 => {"WORD WRAP OFF", ""},
    0x97 => {"SCROLL ON", ""},
    0x98 => {"SCROLL OFF", ""},
    0x99 => {"UNDERLINE START", ""},
    0x9A => {"UNDERLINE STOP", ""},
    0x9B => {"FLASH CURSOR", ""},
    0x9C => {"STEADY CURSOR", ""},
    0x9D => {"CURSOR OFF", ""},
    0x9E => {"BLINK STOP", "stop blinking the drawing color"},
    0x9F => {"UNPROTECT", "unprotect the active field"}
  }

  @definitions %{
    0x80 => {"DEF MACRO", :macro},
    0x81 => {"DEFP MACRO", :macro},
    0x82 => {"DEFT MACRO", :deft},
    0x83 => {"DEF DRCS", :drcs},
    0x84 => {"DEF TEXTURE", :texture}
  }

  # C0 controls other than SI/SO/SS2/SS3/ESC and those with operands (1C, 1F). CR and LF are part
  # of TEXT runs while the primary set is in GL. NAP.txt "C0 Control Set".
  @c0_controls %{
    0x00 => {"NUL", "ignored"},
    0x07 => {"BELL", ""},
    0x08 => {"BACKSPACE", "cursor back one character (APB)"},
    0x09 => {"TAB", "cursor forward one character (APF)"},
    0x0A => {"LINE FEED", "cursor down one row (APD)"},
    0x0B => {"VERTICAL TAB", "cursor up one row (APU)"},
    0x0C => {"CLEAR SCREEN", "clear the screen, cursor to the top left"},
    0x0D => {"CARRIAGE RETURN", "cursor to the start of the line (APR)"},
    0x18 => {"CANCEL", "stop the running macros"},
    0x1A => {"SERVICE DELIMITER", "discarded"},
    0x1E => {"HOME", "cursor to the top left"}
  }

  # G-sets that can be designated (ESC <intermediate> <final>), by final byte. 20 7A / 20 7B are the
  # macro and DRCS finals; the single-byte 7A / 7B are their legacy forms (NAP.txt "Code Sets").
  @set_finals %{
    0x42 => :primary,
    0x7C => :supplementary,
    0x57 => :pdi,
    0x7D => :mosaic,
    0x7A => :macro,
    0x7B => :drcs
  }

  # 28 = G0 (94-character sets); 2D/2E/2F = G1/G2/G3 (96-character sets), with the legacy 29/2A/2B
  # accepted for G1/G2/G3 as NAP.txt asks (GCU writes ESC ) W).
  @set_intermediates %{
    0x28 => 0,
    0x29 => 1,
    0x2A => 2,
    0x2B => 3,
    0x2D => 1,
    0x2E => 2,
    0x2F => 3
  }

  # Locking shifts: ESC <code> invokes a G-set into GL or GR. 6B/6C/6D are the legacy GR forms.
  @locking_shifts %{
    0x6E => {:gl, 2, "LS2"},
    0x6F => {:gl, 3, "LS3"},
    0x7E => {:gr, 1, "LS1R"},
    0x6B => {:gr, 1, "LS1R (legacy)"},
    0x7D => {:gr, 2, "LS2R"},
    0x6C => {:gr, 2, "LS2R (legacy)"},
    0x7C => {:gr, 3, "LS3R"},
    0x6D => {:gr, 3, "LS3R (legacy)"}
  }

  @set_names %{
    primary: "primary (ASCII)",
    pdi: "PDI",
    supplementary: "supplementary",
    mosaic: "mosaic",
    macro: "macro",
    drcs: "DRCS"
  }

  @default_sets {:primary, :pdi, :supplementary, :mosaic}

  @color_palette %{
    0xC0 => "BLACK",
    0xC4 => "RED",
    0xC8 => "DARK GRAY",
    0xCC => "BLUE",
    0xD0 => "GRAY",
    0xD4 => "BROWN",
    0xD8 => "DARK GREEN",
    0xDC => "WHITE",
    0xE0 => "PURPLE BLUE",
    0xE4 => "DARK MAGENTA",
    0xE8 => "MAGENTA",
    0xEC => "ORANGE",
    0xF0 => "YELLOW",
    0xF4 => "GREEN",
    0xF8 => "CYAN",
    0xFC => "DARK CYAN"
  }

  ## RESET fields (NAP.txt RESET)

  @reset_color [
    nil,
    "color 1: mode 0, default palette, white",
    "color 2: color mode and default palette (mode 0 -> as 3)",
    "color 3: mode 1, default palette, white"
  ]

  @reset_screen [
    nil,
    "screen 1: clear to nominal black",
    "screen 2: clear to the drawing color",
    "screen 3: border nominal black",
    "screen 4: border the drawing color",
    "screen 5: screen and border the drawing color",
    "screen 6: screen the drawing color, border nominal black",
    "screen 7: screen and border nominal black"
  ]

  @reset_b2 [{0x01, "text"}, {0x02, "blink"}, {0x04, "fields"}, {0x08, "texture"}] ++
              [{0x10, "macros"}, {0x20, "drcs"}]

  ## Texture attributes

  @fill_patterns [
    "Solid",
    "Vertical hatching",
    "Horizontal hatching",
    "Vertical and horizontal cross-hatching",
    "programmable mask A",
    "programmable mask B",
    "programmable mask C",
    "programmable mask D"
  ]

  @line_textures [
    "Solid",
    "Dotted",
    "Dashed",
    "Dot-dashed"
  ]

  ## Text attributes
  @interchar_spacing [
    "1",
    "1.25",
    "1.5",
    "proportional"
  ]

  @char_path [
    "right",
    "left",
    "up",
    "down"
  ]

  @char_rotation [
    "0°",
    "90°",
    "180°",
    "270°"
  ]

  @cursor_style [
    "underscore",
    "block",
    "cross-hair",
    "custom"
  ]

  @drawing_point [
    "move together",
    "cursor leads",
    "drawing point leads",
    "move independently"
  ]

  @interrow_spacing [
    "1",
    "1.25",
    "1.5",
    "2"
  ]

  # g = the sets in G0..G3; gl / gr = which of them is invoked into GL / GR; definition = the kind
  # of the open DEF ... END (nil outside one); depth indents the listing inside a definition.
  defstruct multi_value_bytes: 3,
            single_value_bytes: 1,
            commands: [],
            g: @default_sets,
            gl: 0,
            gr: 1,
            definition: nil,
            depth: 0

  @doc """
  Check if byte is a data byte (0xC0-0xFF in 8-bit mode)
  """
  def data_byte?(byte), do: Bitwise.band(byte, 0xC0) == 0xC0

  @doc """
  Check if byte is a PDI command (0xA0-0xBF in 8-bit mode, or 0x20-0x3F in 7-bit)
  """
  # 0xA0..0xBF are the normal PDI commands; 0x86 is the REPEAT command.
  def pdi_command?(byte), do: byte in 0xA0..0xBF or byte == 0x86

  @doc """
  Color from the color palette value
  """
  def decode_color([]), do: nil

  def decode_color(data) do
    # Strip bit 7 from all data bytes first
    clean_data = Enum.map(data, &Bitwise.band(&1, 0x7F))

    # Color format: G R B G R B in bits 6,5,4,3,2,1 of each byte
    {g_bits, r_bits, b_bits} =
      Enum.reduce(clean_data, {[], [], []}, fn byte, {g, r, b} ->
        {
          g ++ [Bitwise.band(Bitwise.bsr(byte, 5), 1), Bitwise.band(Bitwise.bsr(byte, 2), 1)],
          r ++ [Bitwise.band(Bitwise.bsr(byte, 4), 1), Bitwise.band(Bitwise.bsr(byte, 1), 1)],
          b ++ [Bitwise.band(Bitwise.bsr(byte, 3), 1), Bitwise.band(byte, 1)]
        }
      end)

    g_val = bits_to_int(g_bits)
    r_val = bits_to_int(r_bits)
    b_val = bits_to_int(b_bits)

    {g_val, r_val, b_val}
  end

  defp bits_to_int(bits) do
    bits
    |> Enum.with_index()
    |> Enum.reduce(0, fn {bit, idx}, acc ->
      acc + Bitwise.bsl(bit, length(bits) - 1 - idx)
    end)
  end

  @doc """
  Collect consecutive data bytes
  """
  def collect_data_bytes(data, pos) do
    collect_data_bytes(data, pos, [])
  end

  defp collect_data_bytes(data, pos, acc) when pos < byte_size(data) do
    <<_skip::binary-size(^pos), byte, _rest::binary>> = data

    if data_byte?(byte) do
      collect_data_bytes(data, pos + 1, acc ++ [byte])
    else
      {acc, pos}
    end
  end

  defp collect_data_bytes(_data, pos, acc), do: {acc, pos}

  @doc """
  Decode coordinates from data bytes.

  Note: XYUtilities.dechunk/1 only handles the 3-byte multi-value format, so the
  multi_value_bytes size tracked in state is ignored here.
  """
  def decode_coordinates(data_bytes, _multi_value_bytes) do
    XYUtilities.dechunk(IO.iodata_to_binary(data_bytes)) |> XYUtilities.mul_256()
  end

  defp coords_to_string([]), do: "None"

  defp coords_to_string(coords) do
    coords
    |> Enum.map(fn {x, y} -> "{#{x},#{y}}" end)
    |> Enum.join(", ")
  end

  @doc """
  Decode a single PDI command
  """
  def decode_pdi(norm_byte, data_bytes, state) do
    cmd_name = Map.get(@pdi_commands, norm_byte, "UNKNOWN_0x#{Integer.to_string(norm_byte, 16)}")
    {params, new_state} = decode_params(norm_byte, data_bytes, state)
    {cmd_name, params, new_state}
  end

  defp decode_params(0xA0, data_bytes, state) do
    # RESET: two fixed bytes; a missing byte counts as all zero bits
    [b1, b2] = Enum.take(data_bytes ++ [0xC0, 0xC0], 2)

    flags =
      [
        if((b1 &&& 0x01) != 0, do: "domain"),
        Enum.at(@reset_color, b1 >>> 1 &&& 0x03),
        Enum.at(@reset_screen, b1 >>> 3 &&& 0x07)
      ] ++ for({bit, name} <- @reset_b2, (b2 &&& bit) != 0, do: name)

    flags = Enum.reject(flags, &is_nil/1)
    params = if flags == [], do: "nothing", else: Enum.join(flags, ", ")
    params = if data_bytes == [], do: params <> " (no data bytes)", else: params

    # A DOMAIN reset puts the operand sizes back to their defaults
    state =
      if (b1 &&& 0x01) != 0,
        do: %{state | multi_value_bytes: 3, single_value_bytes: 1},
        else: state

    {params, state}
  end

  defp decode_params(0xA1, data_bytes, state) do
    # DOMAIN

    if length(data_bytes) > 0 do
      <<_discard::2, xy_or_xyz::1, multi_value_bits::3, single_value_bits::2>> =
        IO.iodata_to_binary([hd(data_bytes)])

      multi_value_size = multi_value_bits + 1
      single_value_size = single_value_bits + 1

      new_state = %{
        state
        | multi_value_bytes: multi_value_size,
          single_value_bytes: single_value_size
      }

      coords_text = if xy_or_xyz == 0, do: "X/Y", else: "X/Y/Z"
      mv_text = "Multi-value size = #{multi_value_size}"
      sv_text = "Single-value size = #{single_value_size}"
      pel_text = "pel size: #{coords_to_string(decode_coordinates(tl(data_bytes), 0))}"
      params = Enum.join([coords_text, mv_text, sv_text, pel_text], ", ")
      {params, new_state}
    else
      {"0 data bytes", state}
    end
  end

  defp decode_params(0xBC, [], state) do
    # SET COLOR with no operand
    {"0 data bytes (transparent)", state}
  end

  defp decode_params(0xBC, data_bytes, state) do
    # SET COLOR: one multi-value operand per palette entry; extra operands set the next entries.
    # decode_color returns {G, R, B} to match the NAPLPS bit layout.
    [first | rest] =
      data_bytes
      |> Enum.chunk_every(state.multi_value_bytes)
      |> Enum.map(fn group ->
        bits =
          if length(group) < state.multi_value_bytes,
            do: " (#{length(group)}-byte operand)",
            else: ""

        "GRB=#{inspect(decode_color(group))}#{bits}"
      end)

    {Enum.join([first | Enum.map(rest, &("next entry " <> &1))], "; "), state}
  end

  defp decode_params(0xBE, [], state) do
    # SELECT COLOR with no operand
    {"no operands: color mode 0", state}
  end

  defp decode_params(0xBE, data_bytes, state) do
    # SELECT COLOR: 1 operand = mode 1 (drawing color); 2 = mode 2 (drawing + background)
    sv = state.single_value_bytes

    if length(data_bytes) >= sv do
      # For Prodigy we want to leave the high bit intact
      palette = Bitwise.band(hd(data_bytes), 0xFF)

      fg =
        "palette_entry=0x#{Integer.to_string(palette, 16)} (#{Map.get(@color_palette, palette)})"

      case Enum.drop(data_bytes, sv) do
        [bg | _] when length(data_bytes) >= 2 * sv ->
          same = if bg == palette, do: " (same entry: the drawing color is not changed)", else: ""

          {"#{fg}, background=#{palette_text(bg)}, mode 2#{same}", state}

        _ ->
          {fg, state}
      end
    else
      {"#{length(data_bytes)} data bytes", state}
    end
  end

  defp decode_params(0xBD, [], state), do: {"0 data bytes", state}

  defp decode_params(0xBD, [fixed | intervals], state) do
    # WAIT: a fixed byte 1011100 in bits 7-1, then intervals in tenths (bits 6-1)
    if (fixed &&& 0x7F) == 0x5C do
      tenths = intervals |> Enum.map(&(&1 &&& 0x3F)) |> Enum.sum()
      {"#{tenths / 10} s (#{tenths} tenths in #{count(intervals, "interval")})", state}
    else
      {"fixed byte 0x#{hex(fixed)} is not 1011100: WAIT discarded", state}
    end
  end

  defp decode_params(0xBF, [], state) do
    {"no data: stop the blinks from the drawing color", state}
  end

  defp decode_params(0xBF, data_bytes, state) do
    # BLINK: blink-to color (single value), then ON, OFF and START DELAY in tenths (bits 6-1);
    # more data starts another blink from the next palette entry.
    sv = state.single_value_bytes

    groups =
      data_bytes
      |> Enum.chunk_every(sv + 3)
      |> Enum.map(fn group ->
        {to, times} = Enum.split(group, sv)
        labels = ["on", "off", "start delay"]

        times =
          Enum.zip(labels, times)
          |> Enum.map(fn {label, t} -> "#{label} #{(t &&& 0x3F) / 10} s" end)

        Enum.join(["to #{palette_text(hd(to))}" | times], ", ")
      end)

    [first | rest] = groups
    {Enum.join([first | Enum.map(rest, &("next blink-from entry: " <> &1))], "; "), state}
  end

  defp decode_params(norm_byte, data_bytes, state)
       when norm_byte in 0xA4..0xB7 do
    # Commands with coordinates (POINT/LINE/ARC/RECT/POLY, abs & rel variants)
    coords = decode_coordinates(data_bytes, state.multi_value_bytes)

    if coords != [] do
      {coords_to_string(coords), state}
    else
      {"#{length(data_bytes)} data bytes", state}
    end
  end

  defp decode_params(0xA2, data_bytes, state) do
    # TEXT ATTRIBUTES
    if length(data_bytes) >= 2 do
      [fix1, fix2 | size_xy] = data_bytes
      <<_discard1::2, char_spacing::2, char_path::2, char_rotation::2>> = <<fix1>>
      <<_discard2::2, cursor_style::2, drawing_point::2, row_spacing::2>> = <<fix2>>

      char_size_text = "Character size: #{coords_to_string(decode_coordinates(size_xy, 0))}"

      sv_text =
        "Inter-character spacing=#{Enum.at(@interchar_spacing, char_spacing)}, " <>
          "Character path=#{Enum.at(@char_path, char_path)}, " <>
          "Character rotation=#{Enum.at(@char_rotation, char_rotation)}, " <>
          "Cursor style=#{Enum.at(@cursor_style, cursor_style)}, " <>
          "Drawing point=#{Enum.at(@drawing_point, drawing_point)}, " <>
          "Inter-row spacing=#{Enum.at(@interrow_spacing, row_spacing)}"

      params =
        Enum.join([sv_text, char_size_text], ", ")

      {params, state}
    else
      {"#{length(data_bytes)} data bytes", state}
    end
  end

  defp decode_params(0xA3, data_bytes, state) do
    # TEXTURE
    if length(data_bytes) >= 1 do
      <<_discard::2, hatching::3, draw_outline::1, line_texture::2>> =
        IO.iodata_to_binary([hd(data_bytes)])

      hatching_text = Enum.at(@fill_patterns, hatching)
      line_texture_text = Enum.at(@line_textures, line_texture)

      sv_text =
        "Fill=#{hatching_text}, Outline=#{if draw_outline == 1, do: "Yes", else: "No"}, Line Texture=#{line_texture_text}"

      mask_text = "Mask size: #{coords_to_string(decode_coordinates(tl(data_bytes), 0))}"
      params = Enum.join([sv_text, mask_text], ", ")
      {params, state}
    else
      {"#{length(data_bytes)} data bytes", state}
    end
  end

  defp decode_params(0xB8, [], state) do
    # FIELD with no data bytes
    {"No data bytes", state}
  end

  defp decode_params(0xB8, data_bytes, state) do
    # FIELD with data bytes
    coords = decode_coordinates(data_bytes, state.multi_value_bytes)
    {"origin, width/height #{coords_to_string(coords)}", state}
  end

  defp decode_params(_norm_byte, data_bytes, state) do
    {"#{length(data_bytes)} data bytes", state}
  end

  # A single-value palette operand: the Prodigy color name and the palette index (the operand's
  # top 4 bits, 16 entries).
  defp palette_text(byte) do
    name = Map.get(@color_palette, byte &&& 0xFF)
    name = if name, do: " (#{name})", else: ""
    "0x#{hex(byte)}#{name} index #{(byte &&& 0x3F) >>> 2}"
  end

  @doc """
  Main decode function
  """
  def decode(filename) do
    filename |> File.read!() |> decode_binary()
  end

  @doc """
  Decode a NAPLPS byte stream (see `decode/1`).
  """
  def decode_binary(data) when is_binary(data) do
    decode_stream(data, 0, %__MODULE__{})
  end

  # Append a listing entry, indented inside a definition body.
  defp add(state, name, params) do
    name = String.duplicate("  ", state.depth) <> name
    %{state | commands: state.commands ++ [{name, params}]}
  end

  defp byte_at(data, pos) when pos < byte_size(data), do: :binary.at(data, pos)
  defp byte_at(_data, _pos), do: nil

  defp gl_set(state), do: elem(state.g, state.gl)
  defp gr_set(state), do: elem(state.g, state.gr)

  defp decode_stream(data, pos, state) when pos < byte_size(data) do
    <<_skip::binary-size(^pos), byte, _rest::binary>> = data

    cond do
      # A DEFT MACRO body is transmit data, not NAPLPS: list it raw up to its terminator
      state.definition == :deft and byte not in 0x80..0x85 ->
        {bytes, new_pos} = take_while(data, pos, &(&1 not in 0x80..0x85))
        new_state = add(state, "TRANSMIT DATA", "#{length(bytes)} bytes: #{hex_list(bytes)}")
        decode_stream(data, new_pos, new_state)

      # C1 controls: definitions, REPEAT, video and text modes
      byte in 0x80..0x9F ->
        {new_pos, new_state} = decode_c1(byte, data, pos + 1, state, "")
        decode_stream(data, new_pos, new_state)

      # Check for PDI commands (8-bit mode: 0xA0-0xBF, 7-bit mode: 0x20-0x3F)
      byte in 0xA0..0xBF and gr_set(state) == :pdi ->
        {data_bytes, new_pos} = collect_data_bytes(data, pos + 1)
        {cmd_name, params, new_state} = decode_pdi(byte, data_bytes, state)

        new_state = add(new_state, cmd_name, params)
        decode_stream(data, new_pos, new_state)

      # Stray data bytes with no command before them: skipped
      gr_set(state) == :pdi and byte >= 0xC0 ->
        decode_stream(data, pos + 1, state)

      # GR holds a character set (LS2R / LS3R): bytes A0-FF are characters from it
      byte >= 0xA0 ->
        {bytes, new_pos} = take_while(data, pos, &(&1 >= 0xA0))
        chars = Enum.map(bytes, &(&1 &&& 0x7F))
        decode_stream(data, new_pos, char_entries(state, gr_set(state), chars, " (GR)"))

      byte == 0x1B ->
        {new_pos, new_state} = decode_escape(data, pos, state)
        decode_stream(data, new_pos, new_state)

      # Shift In (SI) - select G0 (ASCII)
      byte == 0x0F ->
        new_state = add(%{state | gl: 0}, "SHIFT IN", "Select G0 (ASCII)")
        decode_stream(data, pos + 1, new_state)

      # Shift Out (SO) - select G1 (PDI)
      byte == 0x0E ->
        new_state = add(%{state | gl: 1}, "SHIFT OUT", "Select G1 (PDI)")
        decode_stream(data, pos + 1, new_state)

      # Single shifts: the next byte comes from G2 / G3
      byte in [0x19, 0x1D] ->
        {name, g} = if byte == 0x19, do: {"SS2", 2}, else: {"SS3", 3}

        case byte_at(data, pos + 1) do
          c when c in 0x20..0x7F ->
            decode_stream(
              data,
              pos + 2,
              char_entries(state, elem(state.g, g), [c], " (via #{name})")
            )

          _ ->
            decode_stream(data, pos + 1, add(state, name, "no character follows"))
        end

      # Regular ASCII text
      gl_set(state) == :primary and
          (byte == 0x0D or byte == 0x0A or (byte >= 0x20 and byte <= 0x7E)) ->
        {text, new_pos} = collect_text(data, pos)

        if text != "" do
          new_state = add(state, "TEXT", ~s("#{text}"))
          decode_stream(data, new_pos, new_state)
        else
          decode_stream(data, pos + 1, state)
        end

      # DEL in the primary set is discarded
      gl_set(state) == :primary and byte == 0x7F ->
        decode_stream(data, pos + 1, state)

      # Printable bytes from the set in GL: mosaics, supplementary, DRCS, macro calls
      byte in 0x20..0x7F and gl_set(state) == :macro ->
        decode_stream(data, pos + 1, char_entries(state, :macro, [byte], ""))

      byte in 0x20..0x7F ->
        {bytes, new_pos} = take_while(data, pos, &(&1 in 0x20..0x7F))
        decode_stream(data, new_pos, char_entries(state, gl_set(state), bytes, ""))

      true ->
        {new_pos, new_state} = decode_c0(byte, data, pos + 1, state)
        decode_stream(data, new_pos, new_state)
    end
  end

  defp decode_stream(_data, _pos, state), do: state

  defp take_while(data, pos, fun), do: take_while(data, pos, fun, [])

  defp take_while(data, pos, fun, acc) do
    case byte_at(data, pos) do
      b when is_integer(b) ->
        if fun.(b), do: take_while(data, pos + 1, fun, [b | acc]), else: {Enum.reverse(acc), pos}

      nil ->
        {Enum.reverse(acc), pos}
    end
  end

  # Listing entries for printable bytes `bytes` (0x20-0x7F) taken from the G-set `set`.
  defp char_entries(state, :macro, bytes, via) do
    Enum.reduce(bytes, state, fn c, st -> add(st, "MACRO CALL", "code #{char_text(c)}#{via}") end)
  end

  defp char_entries(state, :primary, bytes, via) do
    text = bytes |> Enum.reject(&(&1 == 0x7F)) |> :binary.list_to_bin()
    add(state, "TEXT", ~s("#{text}") <> via)
  end

  defp char_entries(state, :pdi, bytes, via) do
    add(state, "PDI (7-BIT)", "#{length(bytes)} bytes, not decoded: #{hex_list(bytes)}#{via}")
  end

  defp char_entries(state, set, bytes, via) do
    name = set |> Atom.to_string() |> String.upcase()
    add(state, name, "#{count(bytes, "character")}: #{hex_list(bytes)}#{via}")
  end

  defp count([_], noun), do: "1 #{noun}"
  defp count(list, noun), do: "#{length(list)} #{noun}s"

  # C1 control `code`; `pos` is the byte after it. `form` notes the 7-bit ESC form.
  defp decode_c1(code, data, pos, state, form) when code in 0x80..0x84 do
    {name, kind} = Map.fetch!(@definitions, code)
    arg = byte_at(data, pos)

    valid? =
      if kind == :texture, do: arg in 0x41..0x44, else: is_integer(arg) and arg in 0x20..0x7F

    if valid? do
      ends =
        if state.definition, do: " (ends the open #{definition_name(state.definition)})", else: ""

      what =
        if kind == :texture, do: "mask #{<<arg - 0x41 + ?A>>}", else: "code #{char_text(arg)}"

      state = add(%{state | depth: 0}, name, what <> ends <> form)
      {pos + 1, %{state | definition: kind, depth: 1}}
    else
      shown = if arg, do: "0x#{hex(arg)}", else: "missing"
      {pos + 1, add(state, name, "invalid operand #{shown}: both bytes discarded#{form}")}
    end
  end

  defp decode_c1(0x85, _data, pos, state, form) do
    if state.definition do
      ended = definition_name(state.definition)
      {pos, add(%{state | definition: nil, depth: 0}, "END", "end of the #{ended}#{form}")}
    else
      {pos, add(state, "END", "no definition open#{form}")}
    end
  end

  defp decode_c1(0x86, data, pos, state, form) do
    # REPEAT: the count is bits 6-1 of the next byte, which must be 40-7F in bits 7-1
    case byte_at(data, pos) do
      c when is_integer(c) and (c &&& 0x7F) in 0x40..0x7F ->
        count = c &&& 0x3F
        {pos + 1, add(state, "REPEAT", "repeat the previous character #{count} times#{form}")}

      _ ->
        {pos, add(state, "REPEAT", "no count byte: REPEAT discarded#{form}")}
    end
  end

  defp decode_c1(code, _data, pos, state, form) do
    {name, params} = Map.fetch!(@c1_controls, code)
    {pos, add(state, name, params <> form)}
  end

  defp definition_name(:macro), do: "macro definition"
  defp definition_name(:deft), do: "DEFT MACRO"
  defp definition_name(:drcs), do: "DEF DRCS"
  defp definition_name(:texture), do: "DEF TEXTURE"

  # C0 control `byte`; `pos` is the byte after it.
  defp decode_c0(0x1C, data, pos, state) do
    # CURSOR POSITION: row and column bytes, bits 7-1 minus 32 (row 0 = bottom)
    with r when is_integer(r) and r in 0x20..0x7F <- byte_at(data, pos),
         c when is_integer(c) and c in 0x20..0x7F <- byte_at(data, pos + 1) do
      {pos + 2, add(state, "CURSOR POSITION", "row #{r - 32}, column #{c - 32}")}
    else
      _ -> {pos, add(state, "CURSOR POSITION", "no row/column bytes: discarded")}
    end
  end

  defp decode_c0(0x1F, data, pos, state) do
    # NSR: resets the G-sets, GL/GR and DOMAIN; two 40-7F bytes after it place the cursor
    state = %{state | g: @default_sets, gl: 0, gr: 1, multi_value_bytes: 3, single_value_bytes: 1}

    with r when is_integer(r) and r in 0x40..0x7F <- byte_at(data, pos),
         c when is_integer(c) and c in 0x40..0x7F <- byte_at(data, pos + 1) do
      {pos + 2,
       add(state, "NSR", "non-selective reset, cursor to row #{r &&& 0x3F}, column #{c &&& 0x3F}")}
    else
      _ -> {pos, add(state, "NSR", "non-selective reset")}
    end
  end

  defp decode_c0(byte, _data, pos, state) do
    case Map.fetch(@c0_controls, byte) do
      {:ok, {name, params}} ->
        {pos, add(state, name, params)}

      :error ->
        {pos, add(state, "CONTROL 0x#{hex(byte)}", "transmission/device control (ignored)")}
    end
  end

  # An escape sequence at `pos` (the ESC byte). Returns {next_pos, state}.
  defp decode_escape(data, pos, state) do
    b1 = byte_at(data, pos + 1)

    cond do
      b1 == nil ->
        {pos + 1, add(state, "ESCAPE", "ESC at the end of the file")}

      # 7-bit form of a C1 control
      b1 in 0x40..0x5F ->
        decode_c1(b1 + 0x40, data, pos + 2, state, " (7-bit ESC form)")

      Map.has_key?(@locking_shifts, b1) ->
        {side, g, name} = Map.fetch!(@locking_shifts, b1)
        set = @set_names[elem(state.g, g)]
        state = if side == :gl, do: %{state | gl: g}, else: %{state | gr: g}
        where = if side == :gl, do: "GL", else: "GR"

        {pos + 2,
         add(state, "ESCAPE", "#{seq_text([0x1B, b1])}: #{name}, G#{g} (#{set}) into #{where}")}

      Map.has_key?(@set_intermediates, b1) ->
        decode_designation(data, pos, b1, state)

      {b1, byte_at(data, pos + 2)} in [{0x21, 0x4B}, {0x22, 0x46}, {0x25, 0x41}, {0x25, 0x40}] ->
        b2 = byte_at(data, pos + 2)

        what =
          case {b1, b2} do
            {0x21, _} -> "C0 set activated"
            {0x22, _} -> "C1 set activated"
            {0x25, 0x41} -> "start of NAPLPS"
            {0x25, 0x40} -> "end of NAPLPS"
          end

        {pos + 3, add(state, "ESCAPE", "#{seq_text([0x1B, b1, b2])}: #{what}")}

      true ->
        {pos + 2, add(state, "ESCAPE", "#{seq_text([0x1B, b1])}: unknown sequence (discarded)")}
    end
  end

  # ESC <intermediate> [20] <final>: designate a G-set into G0-G3.
  defp decode_designation(data, pos, inter, state) do
    g = Map.fetch!(@set_intermediates, inter)
    f = byte_at(data, pos + 2)

    {final, len} =
      if f == 0x20, do: {byte_at(data, pos + 3), 4}, else: {f, 3}

    set =
      cond do
        f == 0x20 and final in [0x7A, 0x7B] -> Map.fetch!(@set_finals, final)
        f == 0x20 -> nil
        true -> Map.get(@set_finals, final)
      end

    seq = :binary.bin_to_list(data, pos, min(len, byte_size(data) - pos))

    if set do
      legacy =
        if inter in [0x29, 0x2A, 0x2B] and set in [:pdi, :mosaic, :macro, :drcs],
          do: " (legacy intermediate)",
          else: ""

      state = %{state | g: put_elem(state.g, g, set)}

      {pos + len,
       add(state, "ESCAPE", "#{seq_text(seq)}: G#{g} := #{@set_names[set]} set#{legacy}")}
    else
      {pos + len, add(state, "ESCAPE", "#{seq_text(seq)}: unknown set (discarded)")}
    end
  end

  defp seq_text(bytes) do
    bytes
    |> Enum.map(fn
      0x1B -> "ESC"
      0x20 -> "SP"
      b when b in 0x21..0x7E -> <<b>>
      b -> "0x" <> hex(b)
    end)
    |> Enum.join(" ")
  end

  defp char_text(c) when c in 0x21..0x7E, do: "0x#{hex(c)} '#{<<c>>}'"
  defp char_text(c), do: "0x#{hex(c)}"

  defp hex(b), do: b |> Integer.to_string(16) |> String.pad_leading(2, "0")
  defp hex_list(bytes), do: bytes |> Enum.map(&hex/1) |> Enum.join(" ")

  defp collect_text(data, pos) do
    collect_text(data, pos, "")
  end

  defp collect_text(data, pos, acc) when pos < byte_size(data) do
    <<_skip::binary-size(^pos), byte, _rest::binary>> = data

    cond do
      # Display carriage return
      byte == 0x0D ->
        collect_text(data, pos + 1, acc <> "\\r")

      # Display line feed
      byte == 0x0A ->
        collect_text(data, pos + 1, acc <> "\\n")

      byte >= 0x20 and byte <= 0x7E ->
        collect_text(data, pos + 1, acc <> <<byte>>)

      true ->
        {acc, pos}
    end
  end

  defp collect_text(_data, pos, acc), do: {acc, pos}

  @doc """
  Print all decoded commands
  """
  def print_commands(state) do
    IO.puts("\nNAPLPS Commands:")
    IO.puts(String.duplicate("=", 80))

    state.commands
    |> Enum.with_index(1)
    |> Enum.each(fn {{cmd, params}, idx} ->
      IO.puts(
        "#{String.pad_leading(Integer.to_string(idx), 4)}. #{String.pad_trailing(cmd, 20)} #{params}"
      )
    end)

    IO.puts(String.duplicate("=", 80))
    IO.puts("Total commands: #{length(state.commands)}")
  end
end

# Main execution
case System.argv() do
  [filename] ->
    state = NAPLPSDecoder.decode(filename)
    NAPLPSDecoder.print_commands(state)

  _ ->
    IO.puts("Usage: elixir decode_naplps.ex <filename>")
    System.halt(1)
end
