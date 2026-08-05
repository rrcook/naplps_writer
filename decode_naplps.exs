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
  """

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

  defstruct multi_value_bytes: 3,
            single_value_bytes: 1,
            commands: []

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

  defp decode_params(0x86, data_bytes, state) do
    # REPEAT
    if length(data_bytes) >= 1 do
      <<_discard::1, repeat_count::7>> = IO.iodata_to_binary([hd(data_bytes)])
      {"repeat the previous character #{repeat_count} times", state}
    else
      {"#{length(data_bytes)} data bytes", state}
    end
  end

  defp decode_params(0xA1, data_bytes, state) do
    # DOMAIN

    if length(data_bytes) > 0 do
      <<_discard::2, xy_or_xyz::1, multi_value_bits::3, single_value_bits::2>> =
        IO.iodata_to_binary([hd(data_bytes)])

      multi_value_size = multi_value_bits + 1
      single_value_size = single_value_bits + 1

      new_state = %{state | multi_value_bytes: multi_value_size}
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

  defp decode_params(0xBC, data_bytes, state) do
    # SET COLOR
    if length(data_bytes) >= state.multi_value_bytes do
      color_data = Enum.take(data_bytes, state.multi_value_bytes)
      color = decode_color(color_data)
      # decode_color returns {G, R, B} to match the NAPLPS bit layout.
      {"GRB=#{inspect(color)}", state}
    else
      {"#{length(data_bytes)} data bytes", state}
    end
  end

  defp decode_params(0xBE, data_bytes, state) do
    # SELECT COLOR
    if length(data_bytes) >= state.single_value_bytes do
      # For Prodigy we want to leave the high bit intact
      palette = Bitwise.band(hd(data_bytes), 0xFF)

      {"palette_entry=0x#{Integer.to_string(palette, 16)} (#{Map.get(@color_palette, palette)})",
       state}
    else
      {"#{length(data_bytes)} data bytes", state}
    end
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

  @doc """
  Main decode function
  """
  def decode(filename) do
    data = File.read!(filename)
    state = %__MODULE__{}
    decode_stream(data, 0, state)
  end

  defp decode_stream(data, pos, state) when pos < byte_size(data) do
    <<_skip::binary-size(^pos), byte, _rest::binary>> = data

    cond do
      # Check for PDI commands (8-bit mode: 0xA0-0xBF, 7-bit mode: 0x20-0x3F)
      pdi_command?(byte) ->
        {data_bytes, new_pos} = collect_data_bytes(data, pos + 1)
        {cmd_name, params, new_state} = decode_pdi(byte, data_bytes, state)

        new_state = %{new_state | commands: state.commands ++ [{cmd_name, params}]}
        decode_stream(data, new_pos, new_state)

      # Shift In (SI) - select G0 (ASCII)
      byte == 0x0F ->
        new_state = %{state | commands: state.commands ++ [{"SHIFT IN", "Select G0 (ASCII)"}]}
        decode_stream(data, pos + 1, new_state)

      # Shift Out (SO) - select G1 (PDI)
      byte == 0x0E ->
        new_state = %{state | commands: state.commands ++ [{"SHIFT OUT", "Select G1 (PDI)"}]}
        decode_stream(data, pos + 1, new_state)

      # Regular ASCII text
      byte == 0x0D or byte == 0x0A or (byte >= 0x20 and byte <= 0x7E) ->
        {text, new_pos} = collect_text(data, pos)

        if text != "" do
          new_state = %{state | commands: state.commands ++ [{"TEXT", ~s("#{text}")}]}
          decode_stream(data, new_pos, new_state)
        else
          decode_stream(data, pos + 1, state)
        end

      # Skip other bytes
      true ->
        decode_stream(data, pos + 1, state)
    end
  end

  defp decode_stream(_data, _pos, state), do: state

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
