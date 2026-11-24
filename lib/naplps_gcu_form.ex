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

defmodule NaplpsGcuForm do
  @moduledoc """
  GCU's normalized form of a NAPLPS picture: the bytes GCU writes when it loads a file and saves it
  again (SAVE File, ABSOLUTE mode).

  `normalize/2` reads a stream into the objects GCU's reader would make from it, then writes them the
  way GCU's writer does. The rules come from the reconstruction of GCU.EXE (`gcu/src/naplps_read.c`
  for the objects, `gcu/src/naplps_write.c` for the writer) and were checked against GCU itself
  under emulation:

    * every object gets its attribute records before it, and only those that changed: DOMAIN and the
      colour against the previous object, TEXTURE against the previous coordinate object, TEXT
      against the previous text object; a coordinate object after text always gets its colour;
    * lines, rectangles, arcs and polygons are written in their SET& form with an absolute start,
      except that a LINE or RECT that starts where the previous one of the same kind left the pen
      is just the command (the start is left out, and so is the command when it was the last one
      written); several points share one POINT ABS command;
    * LINE REL deltas that follow each other make one object (`A4 start A9 deltas`); every other
      line segment is its own object;
    * text is a FIELD (when the field moved), `A4 start`, then the characters with their colour,
      reverse video, underline and character-set changes written where they change, runs of four
      or more repeats compressed into REPEAT, and the colour left out when it equals the last
      graphic colour;
    * there is no SI in the header, and the file ends with `A5`.

  It handles the streams `TelidrawTranspiler` writes, starting with the `NaplpsWriter.gcu_init/0`
  header. A construct it does not model returns `{:error, reason}` instead of guessing: other
  DOMAIN lengths, INCREMENTAL commands, NSR, text with cursor controls (CR, LF, BS ...) or REPEAT,
  text along a path other than right, and palette changes unless `palette: true` (GCU's DS:8418
  option).
  """

  import Bitwise

  # Code-set escape records by object escape index (GCU DS:8B7E): 1 and 3 write nothing.
  @esc_records %{
    2 => <<0x1B, 0x2A, 0x7C, 0x1B, 0x6E>>,
    4 => <<0x1B, 0x2F, 0x7D, 0x1B, 0x6F>>,
    5 => <<0x1B, 0x2F, 0x7A, 0x1B, 0x6F>>,
    6 => <<0x1B, 0x2F, 0x7B, 0x1B, 0x6F>>,
    7 => <<0x1B, 0x2A, 0x7C, 0x19>>,
    8 => <<0x1B, 0x2F, 0x7A, 0x1D>>
  }
  @restore <<0x1B, 0x29, 0x57, 0x1B, 0x7E>>

  # Where RESET text sends the cursor (GCU, measured), in 1/256 units.
  @text_home {0, 190}
  # GCU's TEXT record after its start-up: spacing 1, character field 6 x 10.
  @start_text_rec <<0xA2, 0xC0, 0xC0>>
  @start_text_size {6, 10}

  # GCU's start-up palette (src/naplps_read.c, G/R/B per entry): a grey ramp 0-7, colours 8-15.
  @start_palette [
    {0, 0, 0},
    {0x2492, 0x2492, 0x2492},
    {0x4924, 0x4924, 0x4924},
    {0x6DB6, 0x6DB6, 0x6DB6},
    {0x9249, 0x9249, 0x9249},
    {0xB6DB, 0xB6DB, 0xB6DB},
    {0xDB6D, 0xDB6D, 0xDB6D},
    {0xFFFF, 0xFFFF, 0xFFFF},
    {0, 0, 0xFFFF},
    {0, 0xBFFF, 0xFFFF},
    {0, 0xFFFF, 0x8000},
    {0x4000, 0xFFFF, 0},
    {0xFFFF, 0xFFFF, 0},
    {0xFFFF, 0x4000, 0},
    {0xFFFF, 0, 0x8000},
    {0xBFFF, 0, 0xFFFF}
  ]

  @doc """
  GCU's normalized form of `bytes`. Options: `palette: true` writes palette changes the way GCU
  does with its DS:8418 option (as SELECT COLOR + SET COLOR strings); without it, as with GCU's
  default configuration, they are dropped and RESET's colour-mode bits are cleared.
  """
  @spec normalize(binary(), keyword()) :: {:ok, binary()} | {:error, String.t()}
  def normalize(bytes, opts \\ []) when is_binary(bytes) do
    palette = Keyword.get(opts, :palette, false)
    objects = read(bytes, palette)
    {:ok, write(objects, palette)}
  catch
    {:unsupported, why} -> {:error, why}
  end

  defp unsupported(why), do: throw({:unsupported, why})

  # ===================================================================== reading: GCU's objects

  defp read(<<0xA1, _::binary>> = bytes, palette) do
    d = decode(bytes, new_state(palette, %{}, 0)) |> close_all()
    Enum.reverse(d.objects)
  end

  defp read(_bytes, _palette),
    do: unsupported("gcu_form needs a complete file starting with the gcu_init header")

  # The decoder's state at GCU's start-up (the gcu_init header then sets DOMAIN and TEXTURE).
  defp new_state(palette, macros, depth) do
    %{
      # the drawing point in GCU's text units (1/64 of a 1/256 unit); pen/1 is its whole part
      cp: {0, 0},
      # GCU's line pen (DS:1D60), where a LINE REL object starts: the screen centre at start-up,
      # :unknown after a macro definition (its body moved it)
      env: {128, 100},
      # the start of the last text object since the last FIELD (GCU's text document), and the
      # custom FIELD's origin when one is in effect
      text_doc: nil,
      field: nil,
      # whether to refuse what GCU's reader misreads (off while a macro plays: no objects then)
      checks: depth == 0,
      domain: <<0xA1, 0xC8>> <> encode({0, 0}),
      texture: <<0xA3, 0xC0>> <> encode({6, 10}),
      color: <<0xBE, 0xDC>>,
      # colour mode (GCU's start-up RESET leaves 1) and the background SELECT COLOR / RESET set
      mode: 1,
      bg_index: 0,
      # the palette definition table (step 9): entry i is `BE <i> BC <rgb>`
      defs: start_defs(),
      text_rec: @start_text_rec,
      text_size_bytes: encode(@start_text_size),
      text: %{fields: [spacing: 0, path: 0, rotation: 0], size: @start_text_size},
      fg: 7,
      bg: nil,
      ul: false,
      rev: false,
      gl: :primary,
      g3: nil,
      esc: 3,
      run: nil,
      text_obj: nil,
      macros: macros,
      depth: depth,
      palette: palette,
      objects: []
    }
  end

  defp decode(<<>>, d), do: d

  defp decode(<<op, rest::binary>>, d) when op in 0xA0..0xBF do
    {operands, rest} = take_operands(rest, [])
    decode(rest, pdi(op, operands, d))
  end

  # DEF MACRO: 80 <code> <body> 85 (the body ends with GCU's A5). GCU records the body's objects;
  # a first LINE REL is recorded from its line pen, so that must be the drawing point.
  defp decode(<<0x80, code, rest::binary>>, d) do
    {body, rest} = until_end(rest, <<>>)
    d = close_all(d)

    if d.checks and first_positioning(body) == 0xA9 and d.env != pen(d),
      do:
        misread(
          "a macro body starting with LINE REL where GCU's line pen is not the drawing point"
        )

    recorded = record(body, d.macros, code)

    obj =
      %{kind: :def_macro, blob: <<0x80, code>> <> body <> <<0x85>>, pen: pen(d), esc: d.esc}
      |> attrs(d)

    decode(rest, %{d | macros: Map.put(d.macros, code, recorded), env: :unknown} |> add(obj))
  end

  # DEF TEXTURE: 84 <mask> <body> 85; GCU's decoder puts the drawing point at (0,0) afterwards
  defp decode(<<0x84, mask, rest::binary>>, d) do
    {body, rest} = until_end(rest, <<>>)
    d = close_all(d)

    obj =
      %{kind: :def_texture, blob: <<0x84, mask>> <> body <> <<0x85>>, pen: {0, 0}, esc: d.esc}
      |> attrs(d)

    decode(rest, %{d | cp: {0, 0}} |> add(obj))
  end

  defp decode(<<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, rest::binary>>, d),
    do: decode(rest, %{d | gl: :macro, g3: :macro})

  defp decode(<<0x1B, 0x2F, 0x7A, 0x1D, code, rest::binary>>, d),
    do: decode(rest, call(code, 8, %{d | g3: :macro, gl: g3_gl(d.gl, :macro)}))

  defp decode(<<0x1B, 0x2F, 0x7D, 0x1B, 0x6F, rest::binary>>, d),
    do: decode(rest, %{d | gl: :mosaic, g3: :mosaic})

  defp decode(<<0x1B, 0x2F, 0x7B, 0x1B, 0x6F, rest::binary>>, d),
    do: decode(rest, %{d | gl: :drcs, g3: :drcs})

  defp decode(<<0x1B, 0x2F, 0x20, 0x7B, 0x1B, 0x6F, rest::binary>>, d),
    do: decode(rest, %{d | gl: :drcs, g3: :drcs})

  defp decode(<<0x1B, 0x6E, rest::binary>>, d), do: decode(rest, %{d | gl: :supplementary})
  defp decode(<<0x1B, 0x29, 0x57, 0x1B, 0x7E, rest::binary>>, d), do: decode(rest, d)

  defp decode(<<0x1B, rest::binary>>, _d),
    do: unsupported("escape sequence ESC #{hex(binary_part(rest, 0, min(2, byte_size(rest))))}")

  defp decode(<<0x0F, rest::binary>>, d), do: decode(rest, %{d | gl: :primary})

  defp decode(<<0x19, c, rest::binary>>, d) when c in 0x20..0x7E,
    do: decode(rest, char(c, :supplementary, d, 7))

  defp decode(<<t, rest::binary>>, d) when t in [0x88, 0x89, 0x99, 0x9A] do
    d =
      case t do
        0x88 -> %{d | rev: true}
        0x89 -> %{d | rev: false}
        0x99 -> %{d | ul: true}
        0x9A -> %{d | ul: false}
      end

    decode(rest, d)
  end

  defp decode(<<c, rest::binary>>, %{gl: :macro} = d) when c in 0x20..0x7F,
    do: decode(rest, call(c, 5, d))

  defp decode(<<c, rest::binary>>, d) when c in 0x20..0x7F do
    set = if d.gl in [:primary, :supplementary, :mosaic, :drcs], do: d.gl, else: :primary
    # DEL prints only in the mosaic and DRCS sets
    if c == 0x7F and set not in [:mosaic, :drcs],
      do: decode(rest, d),
      else: decode(rest, char(c, set, d))
  end

  defp decode(<<0x1F, _::binary>>, _d), do: unsupported("NSR")
  defp decode(<<0x86, _::binary>>, _d), do: unsupported("REPEAT in the input")

  defp decode(<<c, _::binary>>, _d) when c < 0x20,
    do: unsupported("control #{hex(<<c>>)} (cursor controls in text are not modelled)")

  defp decode(<<c, _::binary>>, _d), do: unsupported("byte #{hex(<<c>>)}")

  defp take_operands(<<b, rest::binary>>, acc) when b >= 0xC0, do: take_operands(rest, [b | acc])
  defp take_operands(rest, acc), do: {Enum.reverse(acc), rest}

  # The first PDI in a body that is not an attribute (DOMAIN, TEXT, TEXTURE, SELECT COLOR).
  defp first_positioning(<<op, rest::binary>>) when op in [0xA1, 0xA2, 0xA3, 0xBE],
    do: first_positioning(rest)

  defp first_positioning(<<op, _::binary>>) when op in 0xA0..0xBF, do: op
  defp first_positioning(<<_, rest::binary>>), do: first_positioning(rest)
  defp first_positioning(<<>>), do: nil

  defp misread(what), do: unsupported(what <> " (GCU reads it as something else)")

  defp until_end(<<0x85, rest::binary>>, acc), do: {acc, rest}
  defp until_end(<<b, rest::binary>>, acc), do: until_end(rest, acc <> <<b>>)
  defp until_end(<<>>, _acc), do: unsupported("definition without its END (85)")

  # ---- PDI commands

  # SELECT COLOR does not end text (its colour goes into the characters), nor does SET COLOR (its
  # palette change reaches the text object, which takes the definition table when it closes);
  # every other command does. Both end a LINE REL run in apply_select_color: SET COLOR after its
  # palette entry is set (the run takes the new table), SELECT COLOR before its colour changes.
  defp pdi(0xBC, operands, d), do: finish_run(geometry(0xBC, operands, d))

  defp pdi(0xBE, operands, d) do
    d = finish_run(d)

    case operands do
      [fg] ->
        select(%{d | mode: 1, bg_index: 0}, color_index(fg))

      # GCU takes the foreground only when it differs from the background (h_select_color)
      [fgb, bgb] ->
        bg = color_index(bgb)
        fg = if bg != color_index(fgb), do: color_index(fgb), else: d.fg
        select(%{d | mode: 2, bg_index: bg}, fg)

      _ ->
        unsupported("SELECT COLOR with #{length(operands)} operands")
    end
  end

  defp pdi(op, operands, d) do
    d = flush_text(d)
    d = if op == 0xA9, do: d, else: finish_run(d)
    op |> geometry(operands, d) |> env_after(op, operands)
  end

  # apply_SELECT_COLOR: the colour record from the foreground, and the background in mode 2.
  defp select(d, fg) do
    bg = if d.mode == 2, do: d.bg_index, else: nil
    record = if bg, do: <<0xBE, color_byte(fg), color_byte(bg)>>, else: <<0xBE, color_byte(fg)>>
    %{d | color: record, fg: fg, bg: bg}
  end

  # GCU's line pen after a command (src/naplps_read.c b_move_pen / b_point / b_line_seg / b_arc /
  # b_rect / set_field / reset_object): at the drawing point after positioning commands; a
  # polygon leaves it at its last vertex (poly/3); attributes and WAIT/BLINK leave it alone.
  defp env_after(d, op, _ops) when op in 0xA4..0xB3, do: %{d | env: pen(d)}

  defp env_after(d, 0xB8, ops) do
    field = if ops == [], do: nil, else: List.to_tuple(decode_xys(ops))
    %{d | env: pen(d), text_doc: nil, field: field}
  end

  defp env_after(d, 0xA0, [_b1, b2]) when (b2 &&& 1) == 1, do: %{d | env: pen(d)}
  defp env_after(d, _op, _ops), do: d

  # RESET (src/naplps_read.c h_reset): DOMAIN, the palette (bits 1-2: back to the start-up colours,
  # foreground 7, colour mode 0 or 1), the background from the screen bits 3-5, the colour record
  # re-applied; TEXT and macros from the second byte. GCU stores the first byte without the palette
  # bits unless its DS:8418 option is set.
  defp geometry(0xA0, [b1, b2], d) do
    d = if (b1 &&& 0x01) == 1, do: %{d | domain: <<0xA1, 0xC8>> <> encode({0, 0})}, else: d

    d =
      case b1 >>> 1 &&& 3 do
        0 -> d
        1 -> %{d | defs: start_defs(), mode: 0, fg: 7}
        _ -> %{d | defs: start_defs(), mode: 1, fg: 7}
      end

    d =
      case b1 >>> 3 &&& 7 do
        s when s in [1, 7] -> %{d | bg_index: 0}
        s when s in [2, 5, 6] -> %{d | bg_index: d.fg}
        _ -> d
      end

    d = select(d, d.fg)
    b1 = if d.palette, do: b1, else: b1 &&& 0xF9

    d =
      if (b2 &&& 0x01) == 1 do
        %{
          d
          | cp: fine(@text_home),
            text_rec: @start_text_rec,
            text_size_bytes: encode({6, 10}),
            text: %{fields: [spacing: 0, path: 0, rotation: 0], size: {0x199 / 64, 0x280 / 64}},
            ul: false,
            rev: false
        }
      else
        d
      end

    d = if (b2 &&& 0x10) != 0, do: %{d | macros: %{}}, else: d
    control(0xA0, <<b1, b2>>, d)
  end

  defp geometry(0xA1, [fixed | pel], d) do
    if (fixed >>> 2 &&& 7) != 2 or (fixed &&& 3) != 0,
      do: unsupported("DOMAIN other than multi-value 3, single-value 1")

    pel_bytes = if pel == [], do: binary_part(d.domain, 2, 3), else: encode(hd(decode_xys(pel)))
    %{d | domain: <<0xA1, 0xC8>> <> pel_bytes}
  end

  defp geometry(0xA2, operands, d) do
    {b1, b2, size} =
      case operands do
        [] -> {0xC0, 0xC0, []}
        [b1] -> {b1, 0xC0, []}
        [b1, b2 | size] -> {b1, b2, size}
      end

    fields = [spacing: b1 >>> 4 &&& 3, path: b1 >>> 2 &&& 3, rotation: b1 &&& 3]

    {size_bytes, cells} =
      case size do
        [] ->
          {d.text_size_bytes, d.text.size}

        _ ->
          {w, h} = hd(decode_xys(size))
          {encode({abs(w), abs(h)}), {w, h}}
      end

    %{
      d
      | text_rec: <<0xA2, b1, b2>>,
        text_size_bytes: size_bytes,
        text: %{fields: fields, size: cells}
    }
  end

  defp geometry(0xA3, [fixed | mask], d) do
    mask_bytes =
      if mask == [], do: binary_part(d.texture, 2, 3), else: encode(abs_xy(hd(decode_xys(mask))))

    %{d | texture: <<0xA3, 0xC0 ||| (fixed &&& 0x3F)>> <> mask_bytes}
  end

  # POINT SET ABS / REL: the drawing point moves, no object
  defp geometry(0xA4, ops, d),
    do: Enum.reduce(decode_xys(ops), d, fn v, d -> %{d | cp: fine(v)} end)

  defp geometry(0xA5, ops, d), do: Enum.reduce(decode_xys(ops), d, &move_by(&2, &1))

  # POINT ABS / REL: one point object per vertex
  defp geometry(0xA6, ops, d),
    do: Enum.reduce(decode_xys(ops), d, fn v, d -> point(%{d | cp: fine(v)}) end)

  defp geometry(0xA7, ops, d), do: Enum.reduce(decode_xys(ops), d, &point(move_by(&2, &1)))

  # LINE ABS: one object per segment
  defp geometry(0xA8, ops, d), do: Enum.reduce(decode_xys(ops), d, &segment(&2, &1))

  # LINE REL: the deltas join the open line run
  defp geometry(0xA9, ops, d), do: Enum.reduce(decode_xys(ops), d, &run_delta(&2, &1))

  # SET & LINE ABS / REL: GCU reads the operands as (set point, line) pairs
  defp geometry(0xAA, ops, d) do
    decode_xys(ops)
    |> Enum.chunk_every(2)
    |> Enum.reduce(d, fn
      [s, v], d -> segment(%{d | cp: fine(s)}, v)
      [s], d -> %{d | cp: fine(s)}
    end)
  end

  defp geometry(0xAB, ops, d) do
    decode_xys(ops)
    |> Enum.chunk_every(2)
    |> Enum.reduce(d, fn
      [s, {dx, dy}], d -> segment(%{d | cp: fine(s)}, add_xy(s, {dx, dy}))
      [s], d -> %{d | cp: fine(s)}
    end)
  end

  defp geometry(op, ops, d) when op in [0xAC, 0xAD] do
    case decode_xys(ops) do
      [m, e] -> arc(op, m, e, d)
      _ -> unsupported("ARC with #{div(length(ops), 3)} operands")
    end
  end

  defp geometry(op, ops, d) when op in [0xAE, 0xAF] do
    case decode_xys(ops) do
      [s, m, e] -> arc(op - 2, m, e, %{d | cp: fine(s)})
      _ -> unsupported("SET & ARC with #{div(length(ops), 3)} operands")
    end
  end

  defp geometry(op, ops, d) when op in [0xB0, 0xB1],
    do: Enum.reduce(decode_xys(ops), d, &rect(op, &1, &2))

  defp geometry(op, ops, d) when op in [0xB2, 0xB3] do
    decode_xys(ops)
    |> Enum.chunk_every(2)
    |> Enum.reduce(d, fn
      [s, size], d -> rect(op - 2, size, %{d | cp: fine(s)})
      [s], d -> %{d | cp: fine(s)}
    end)
  end

  defp geometry(op, ops, d) when op in [0xB4, 0xB5], do: poly(op, decode_xys(ops), d)

  defp geometry(op, ops, d) when op in [0xB6, 0xB7] do
    case decode_xys(ops) do
      [s | deltas] -> poly(op - 2, deltas, %{d | cp: fine(s)})
      [] -> d
    end
  end

  # FIELD: the drawing point goes to the field's origin, (0,0) for a full-screen field
  defp geometry(0xB8, [], d), do: %{d | cp: {0, 0}}
  defp geometry(0xB8, ops, d), do: %{d | cp: fine(hd(decode_xys(ops)))}

  # SET COLOR: in colour modes 1 and 2 the foreground's palette entry gets the RGB (GCU keeps the
  # first 3 bytes, mv 3); the definition table records it only with GCU's DS:8418 option. In mode 0
  # GCU searches the palette and changes the foreground, which is not modelled.
  defp geometry(0xBC, ops, d) do
    cond do
      d.mode == 0 ->
        unsupported("SET COLOR in colour mode 0 (GCU's palette search is not modelled)")

      length(ops) > 3 ->
        unsupported("SET COLOR with more than one RGB group")

      d.palette ->
        %{d | defs: List.replace_at(d.defs, d.fg, def_entry(d.fg, rgb3(ops)))}

      true ->
        d
    end
  end

  # WAIT: GCU stores the fixed byte as DC and the interval as its low six bits
  defp geometry(0xBD, [_fixed, t], d), do: control(0xBD, <<0xDC, 0xC0 ||| (t &&& 0x3F)>>, d)

  defp geometry(0xBF, ops, d) when length(ops) == 4,
    do: control(0xBF, :binary.list_to_bin(ops), d)

  defp geometry(op, ops, _d),
    do: unsupported("command #{hex(<<op>>)} with #{length(ops)} operand bytes")

  # ---- objects

  defp point(d) do
    obj = %{kind: :coord, cmd: 0xA7, start: pen(d), deltas: [], esc: 3, pen: pen(d)} |> attrs(d)
    add(%{d | esc: 3}, obj)
  end

  defp segment(d, {_, _} = v) do
    start = pen(d)
    d = %{d | cp: fine(v)}
    obj = %{kind: :coord, cmd: 0xA9, start: start, deltas: [sub_xy(v, start)], esc: 3, pen: v}
    add(%{d | esc: 3}, attrs(obj, d))
  end

  # A LINE REL run: one object while the deltas follow each other (at most 254 of them). GCU starts
  # the object at its line pen, so a run where that is not the drawing point is a different line.
  defp run_delta(%{run: nil} = d, delta) do
    if d.checks and d.env != pen(d),
      do: misread("a LINE REL where GCU's line pen is not the drawing point")

    run = %{kind: :coord, cmd: 0xA9, start: pen(d), deltas: [], esc: 3}
    run_delta(%{d | run: run, esc: 3}, delta)
  end

  defp run_delta(%{run: %{deltas: ds}} = d, delta) when length(ds) == 254,
    do: d |> finish_run() |> run_delta(delta)

  defp run_delta(%{run: run} = d, delta) do
    d = move_by(d, delta)
    %{d | run: %{run | deltas: run.deltas ++ [delta]}}
  end

  defp finish_run(%{run: nil} = d), do: d

  defp finish_run(%{run: run} = d),
    do: add(%{d | run: nil}, run |> Map.put(:pen, pen(d)) |> attrs(d))

  defp arc(op, m, e, d) do
    start = pen(d)
    mid = add_xy(start, m)
    stop = add_xy(mid, e)
    # a full circle (the end back at the start) keeps only the delta to the middle
    deltas = if stop == start, do: [m], else: [m, e]
    obj = %{kind: :coord, cmd: op, start: start, deltas: deltas, esc: 3, pen: stop}
    add(%{d | cp: fine(stop), esc: 3}, attrs(obj, d))
  end

  defp rect(op, {w, _h} = size, d) do
    start = pen(d)
    stop = add_xy(start, {w, 0})
    obj = %{kind: :coord, cmd: op, start: start, deltas: [size], esc: 3, pen: stop}
    {cx, cy} = d.cp
    add(%{d | cp: {cx + w * 64, cy}, esc: 3}, attrs(obj, d))
  end

  # A polygon starts at the drawing point, and the drawing point goes back there afterwards; GCU's
  # line pen stays at the last vertex. GCU skips zero deltas (h_poly / h_setpoly `if (FX || FY)`);
  # a polygon with none gets one vertex at the start when the command ends.
  defp poly(op, deltas, d) do
    deltas =
      case Enum.reject(deltas, &(&1 == {0, 0})),
        do: (
          [] -> [{0, 0}]
          ds -> ds
        )

    obj = %{kind: :coord, cmd: op, start: pen(d), deltas: deltas, esc: 3, pen: pen(d)}
    last = Enum.reduce(deltas, pen(d), &add_xy(&2, &1))
    add(%{d | esc: 3, env: last}, attrs(obj, d))
  end

  defp control(cmd, blob, d) do
    obj = %{kind: :control, cmd: cmd, blob: blob, esc: d.esc, pen: pen(d)} |> attrs(d)
    add(d, obj)
  end

  # A macro call: the call object at the drawing point, then the macro plays (no objects), which
  # moves the drawing point the way its recording does.
  defp call(code, esc, d) do
    d = close_all(d)
    start = pen(d)
    obj = %{kind: :call, code: code, esc: esc, start: start} |> attrs(d)

    d =
      case Map.fetch(d.macros, code) do
        {:ok, recorded} -> %{d | cp: fine(play_pen(recorded, start, d.macros, d.depth + 1))}
        :error -> d
      end

    # GCU's line pen is the drawing point after a call (measured)
    add(%{d | esc: esc, env: pen(d)}, Map.put(obj, :pen, pen(d)))
  end

  # A text character: into the open text object (opened at the drawing point), then the cursor
  # moves by GCU's advance for it. The object's escape index is its last character's set (an SS2
  # character's is 7).
  defp char(c, set, d, esc \\ nil) do
    if d.checks and d.run != nil and d.text_obj == nil,
      do: misread("text right after a LINE REL run (GCU loses the run's deltas)")

    d = finish_run(d)

    if d.text.fields[:path] != 0,
      do: unsupported("text along a path other than right (GCU's field for it is not modelled)")

    d = if d.text_obj, do: d, else: open_text(d)

    cell = {c, set, d.fg, d.bg, d.ul, d.rev}
    {dx, dy} = NaplpsText.gcu_text_advance(<<c>>, set, d.text.fields, d.text.size)
    {cx, cy} = d.cp
    esc = esc || %{primary: 1, supplementary: 2, mosaic: 4, drcs: 6}[set]

    # GCU wraps a line at the right edge of the screen, or of a custom FIELD (a CR, and a FIELD
    # with operands)
    right =
      case d.field,
        do: (
          {{fx, _}, {fw, _}} -> fx + fw
          nil -> 256
        )

    if d.checks and cx + round(dx * 64) > right * 64,
      do: unsupported("text past the right edge of its field (GCU wraps it onto a new line)")

    %{
      d
      | cp: {cx + round(dx * 64), cy + round(dy * 64)},
        esc: esc,
        text_obj: %{d.text_obj | cells: d.text_obj.cells ++ [cell], esc: esc}
    }
  end

  defp flush_text(%{text_obj: nil} = d), do: d

  defp flush_text(%{text_obj: t} = d) do
    obj =
      Map.merge(t, %{
        pen: pen(d),
        domain: d.domain,
        defs: d.defs,
        text_rec: d.text_rec <> d.text_size_bytes,
        rotation: d.text_rec |> :binary.at(1) |> band(3)
      })

    add(%{d | text_obj: nil}, obj)
  end

  # A new text object at the cursor. GCU makes none when the line does not fit on the screen
  # (src/gcu_text.c layout/fits: the line from y to y + line height, x up to the right edge). Its
  # text document lives on until a FIELD: a new object on the row of the previous one's start, to
  # its right, is padded with spaces up to the cursor, and GCU's save writes them after the cursor,
  # which moves the text (measured).
  defp open_text(d) do
    {x, y} = start = pen(d)
    {_w, h} = d.text.size

    if d.checks and (x < 0 or x >= 256 or y < 0 or y + h > 200),
      do: unsupported("text whose line is not on the screen (GCU drops or moves it)")

    if d.checks and d.text.fields[:rotation] != 0,
      do: unsupported("rotated text (GCU's field for it is not modelled)")

    # In a custom FIELD GCU pads a new text object with spaces from the field's left edge when it
    # starts a whole number of cells from it (measured), and wraps at the field's right edge.
    case d.field do
      {{fx, _fy}, _size} when d.checks and x != fx ->
        unsupported("text in a custom FIELD away from its left edge (GCU pads it with spaces)")

      _ ->
        :ok
    end

    case d.text_doc do
      {ox, ^y} when d.checks and x > ox ->
        misread(
          "text on the row of the previous text object, to its right, with no FIELD between"
        )

      _ ->
        :ok
    end

    %{d | text_obj: %{kind: :text, start: start, cells: [], esc: 1}, env: start, text_doc: start}
  end

  defp close_all(d), do: d |> flush_text() |> finish_run()

  defp attrs(obj, d),
    do: Map.merge(obj, %{domain: d.domain, texture: d.texture, color: d.color, defs: d.defs})

  defp add(d, obj), do: %{d | objects: [obj | d.objects]}

  # An SS3 call re-designates G3; when G3 was in GL, GL now holds the macro set.
  defp g3_gl(gl, set) when gl in [:mosaic, :drcs, :macro], do: set
  defp g3_gl(gl, _set), do: gl

  # ---- the palette definition table (src/naplps_read.c palette_set, src/naplps_write.c o0688)

  # Entry i: SELECT COLOR i, SET COLOR with the RGB in mv-3 bytes (2 bits of G, R, B per byte).
  defp def_entry(i, rgb), do: <<0xBE, color_byte(i), 0xBC>> <> rgb

  # GCU's table after its start-up: every entry with its start-up colour (its start-up RESET).
  defp start_defs do
    @start_palette
    |> Enum.with_index()
    |> Enum.map(fn {{g, r, b}, i} -> def_entry(i, rgb_encode(g, r, b)) end)
  end

  # rgb_encode I 0x18d71: three bytes, two bits of each 16-bit component per byte, MSB first.
  defp rgb_encode(g, r, b) do
    for k <- 0..2, into: <<>> do
      hi = 15 - 2 * k
      lo = hi - 1

      <<0xC0 ||| (g >>> hi &&& 1) <<< 5 ||| (r >>> hi &&& 1) <<< 4 ||| (b >>> hi &&& 1) <<< 3 |||
          (g >>> lo &&& 1) <<< 2 ||| (r >>> lo &&& 1) <<< 1 ||| (b >>> lo &&& 1)>>
    end
  end

  # SET COLOR's operand bytes as GCU keeps them: the same bits, three bytes (missing ones zero).
  defp rgb3(ops) do
    bytes = Enum.map(ops, &(0xC0 ||| (&1 &&& 0x3F)))
    :binary.list_to_bin(bytes ++ List.duplicate(0xC0, 3 - length(bytes)))
  end

  defp color_byte(i), do: 0xC0 ||| i <<< 2

  # ---- macros: GCU's recording and where playing it leaves the drawing point

  # GCU records a nested call's playback too: after a call's code byte comes the called macro's
  # own recording (the macro being defined and undefined ones are not played).
  defp record(body, macros, self, depth \\ 0)
  defp record(<<>>, _m, _self, _depth), do: <<>>

  defp record(<<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, code, rest::binary>>, m, self, depth),
    do:
      <<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, code>> <>
        played(code, m, self, depth) <> record(rest, m, self, depth)

  defp record(<<0x1B, 0x2F, 0x7A, 0x1D, code, rest::binary>>, m, self, depth),
    do:
      <<0x1B, 0x2F, 0x7A, 0x1D, code>> <>
        played(code, m, self, depth) <> record(rest, m, self, depth)

  defp record(<<b, rest::binary>>, m, self, depth), do: <<b>> <> record(rest, m, self, depth)

  defp played(code, _m, code, _depth), do: <<>>

  defp played(code, m, self, depth) when depth < 16 do
    case Map.fetch(m, code) do
      {:ok, rec} -> record(rec, m, self, depth + 1)
      :error -> <<>>
    end
  end

  defp played(_code, _m, _self, _depth), do: <<>>

  # The drawing point after playing `bytes` from `pen`: the decoder above run on a scratch state
  # (GCU records no objects while a macro plays). Depth-limited: GCU hangs on a macro that calls
  # itself while it plays.
  defp play_pen(bytes, pen, macros, depth) when depth < 16 do
    scratch = %{new_state(true, macros, depth) | cp: fine(pen)}
    pen(decode(bytes, scratch) |> close_all())
  end

  defp play_pen(_bytes, pen, _macros, _depth), do: pen

  # ===================================================================== writing: GCU's ABSOLUTE writer

  defp write(objects, palette) do
    w = %{
      out: [],
      prev: nil,
      prev_coord: nil,
      prev_text: nil,
      last_op: 0,
      last_field_x: -1,
      esc_seen: true,
      tog88: false,
      tog99: false,
      ref_color: <<>>,
      first_call: true,
      drv_pen: {0, 0}
    }

    w = Enum.reduce(objects, w, &write_and_track(&1, &2, palette))
    IO.iodata_to_binary(Enum.reverse(w.out)) <> <<0xA5>>
  end

  # GCU asks its MVDI driver where the drawing point is after re-sending it the previous object;
  # it never re-sends a macro call (measured: the pen it uses after a call is the one from before
  # it), so after a call its pen is stale. drv_pen keeps that pen; the decoder's true pen is the
  # previous object's own. Every object starts with the palette entries that changed (o0a17,
  # with the DS:8418 option only: without it GCU's table stays empty).
  defp write_and_track(o, w, palette) do
    w = if palette, do: defs_rec(w, o), else: w
    w = write_object(o, w)
    if o.kind == :call, do: w, else: %{w | drv_pen: o.pen}
  end

  # ovl2:0x688: the entries that differ from the previous object's table (the start-up table
  # before the first object), each `BE <i> BC <rgb>`.
  defp defs_rec(w, o) do
    ref = if w.prev, do: w.prev.defs, else: start_defs()

    Enum.zip(o.defs, ref)
    |> Enum.reduce(w, fn
      {same, same}, w -> w
      {entry, _}, w -> put(w, entry)
    end)
  end

  # The decoder's drawing point after the previous object.
  defp driver_pen(%{prev: nil}), do: {0, 0}
  defp driver_pen(%{prev: p}), do: p.pen

  defp write_object(%{kind: :control, cmd: 0xBF} = o, w) do
    w = %{w | last_op: 0} |> domain_rec(o) |> esc_rec(o) |> texture_rec(o)
    w = w |> put(o.color) |> put(<<0xBF>> <> o.blob)
    %{w | prev_coord: o, prev: o}
  end

  defp write_object(%{kind: :control} = o, w) do
    w = %{w | last_op: 0} |> domain_rec(o) |> esc_rec(o) |> texture_rec(o) |> color_rec(o)
    w = put(w, <<o.cmd>> <> o.blob)
    %{w | prev_coord: o, prev: o}
  end

  defp write_object(%{kind: :def_texture} = o, w) do
    w = %{w | last_op: 0} |> domain_rec(o) |> texture_rec(o) |> color_rec(o)
    w = w |> put(o.blob) |> put(@restore)
    %{w | prev_coord: o, prev: o}
  end

  defp write_object(%{kind: :def_macro} = o, w) do
    w = %{w | last_op: 0} |> domain_rec(o) |> put(o.blob)
    %{w | prev: o}
  end

  defp write_object(%{kind: :call} = o, w) do
    # Positioned from the decoder's true pen (GCU uses its stale driver pen here and misplaces
    # every call after the first; measured).
    {px, py} = driver_pen(w)
    w = %{w | last_op: 0} |> domain_rec(o)

    position =
      if w.first_call,
        do: <<0xA4>> <> encode(o.start),
        else: <<0xA5>> <> encode(sub_xy(o.start, {px, py}))

    w =
      w
      |> put(position)
      |> put(Map.fetch!(@esc_records, o.esc))
      |> put(<<o.code>>)
      |> put(@restore)

    %{w | first_call: false, prev: o}
  end

  defp write_object(%{kind: :coord} = o, w) do
    # A start is left out only where GCU's pen and the decoder's agree (they differ only after a
    # macro call), so the shortcut never moves a drawing.
    pen = if w.drv_pen == driver_pen(w), do: w.drv_pen, else: :unknown
    w = w |> domain_rec(o) |> esc_rec(o) |> texture_rec(o) |> color_rec(o) |> command(o, pen)
    %{w | prev_coord: o, prev: o}
  end

  defp write_object(%{kind: :text} = o, w) do
    w = w |> domain_rec(o) |> text_rec(o)
    {x0, _y0} = o.start

    w =
      if o.rotation != 0 or x0 != w.last_field_x do
        # a FIELD before the text; its operands only when the text holds a CR (not modelled)
        put(%{w | last_field_x: if(o.rotation != 0, do: -1, else: 0)}, <<0xB8>>)
      else
        w
      end

    w = %{w | last_op: 0} |> put(<<0xA4>> <> encode(o.start))
    w = text_blob(w, serialize_cells(o.cells))
    %{w | prev_text: o, prev: o}
  end

  # ---- attribute records (written only when they changed)

  defp domain_rec(%{prev: nil} = w, o), do: put(w, o.domain)

  defp domain_rec(%{prev: p} = w, o) do
    cond do
      p.domain == o.domain ->
        w

      binary_part(p.domain, 2, 3) == binary_part(o.domain, 2, 3) ->
        put(w, binary_part(o.domain, 0, 2))

      true ->
        put(w, o.domain)
    end
  end

  defp texture_rec(%{prev_coord: nil} = w, o), do: put(w, o.texture)

  defp texture_rec(%{prev_coord: p} = w, o) do
    cond do
      p.texture == o.texture ->
        w

      binary_part(p.texture, 2, 3) == binary_part(o.texture, 2, 3) ->
        put(w, binary_part(o.texture, 0, 2))

      true ->
        put(w, o.texture)
    end
  end

  # The colour always after a TEXT object (or with no coordinate object before), else on change.
  defp color_rec(%{prev_coord: nil} = w, o), do: put(w, o.color)
  defp color_rec(%{prev: %{kind: :text}} = w, o), do: put(w, o.color)

  defp color_rec(%{prev_coord: p} = w, o),
    do: if(p.color == o.color, do: w, else: put(w, o.color))

  # TEXT: the first text object writes its whole record (and resets GCU's text writer); later
  # ones the opcode + fixed bytes when the record changed, the size only when it changed.
  defp text_rec(%{prev_text: nil} = w, o) do
    w = %{w | tog88: false, tog99: false, esc_seen: true, ref_color: <<>>, last_field_x: -1}
    put(w, o.text_rec)
  end

  defp text_rec(%{prev_text: p} = w, o) do
    cond do
      p.text_rec == o.text_rec ->
        w

      binary_part(p.text_rec, 3, 3) == binary_part(o.text_rec, 3, 3) ->
        put(w, binary_part(o.text_rec, 0, 3))

      true ->
        put(w, o.text_rec)
    end
  end

  defp esc_rec(%{prev: p} = w, o) do
    if p && p.esc == o.esc, do: w, else: put(w, Map.get(@esc_records, o.esc, <<>>))
  end

  # ---- coordinate commands (ovl2:0x2514)

  defp command(w, %{cmd: 0xA7} = o, _pen) do
    w = if w.last_op != 0xA6, do: put_op(w, 0xA6), else: w
    put(w, encode(o.start), false)
  end

  defp command(w, %{cmd: cmd} = o, pen) when cmd in [0xA9, 0xB0, 0xB1] do
    prefix = length(o.deltas) > 1
    op = if prefix, do: 0xA4, else: cmd + 2
    after_op = if prefix, do: <<cmd>>, else: <<>>

    w =
      cond do
        w.prev_coord == nil or w.prev_coord[:cmd] != cmd ->
          w |> put_op(op) |> put(encode(o.start) <> after_op, false)

        o.start == pen ->
          if w.last_op != cmd, do: put_op(w, cmd), else: w

        true ->
          w = if w.last_op != op or prefix, do: put_op(w, op), else: w
          put(w, encode(o.start) <> after_op, false)
      end

    put(w, encode_all(o.deltas), false)
  end

  defp command(w, %{cmd: cmd} = o, _pen) do
    w
    |> put_op(cmd + 2)
    |> Map.put(:last_op, cmd)
    |> put(encode(o.start) <> encode_all(o.deltas), false)
  end

  # ---- text (ovl4:0x2200 cells -> blob, then the writer's I 0x25c68 filter)

  defp serialize_cells(cells) do
    {out, last, rep} =
      Enum.reduce(cells, {[], nil, 0}, fn cell, {out, last, rep} ->
        if last != nil and cell == last do
          {out, last, rep + 1}
        else
          out = if rep > 0, do: [repeat(elem(last, 0), rep) | out], else: out
          {[cell_bytes(cell, last) | out], cell, 0}
        end
      end)

    out = if rep > 0, do: [repeat(elem(last, 0), rep) | out], else: out
    IO.iodata_to_binary(Enum.reverse(out))
  end

  defp cell_bytes({c, set, fg, bg, ul, rev}, last) do
    first = last == nil
    {_, lset, lfg, lbg, lul, lrev} = last || {nil, nil, nil, nil, nil, nil}
    color = if first or fg != lfg or bg != lbg, do: color_bytes(fg, bg), else: <<>>
    u = if first or ul != lul, do: if(ul, do: <<0x99>>, else: <<0x9A>>), else: <<>>
    r = if first or rev != lrev, do: if(rev, do: <<0x88>>, else: <<0x89>>), else: <<>>
    s = if first or set != lset, do: NaplpsWriter.charset_switch(<<>>, set), else: <<>>
    color <> u <> r <> s <> <<c>>
  end

  defp color_bytes(fg, nil), do: <<0xBE, 0xC0 ||| fg <<< 2>>
  defp color_bytes(fg, bg), do: <<0xBE, 0xC0 ||| fg <<< 2, 0xC0 ||| bg <<< 2>>

  # c repeated n more times: n < 4 literally, else C1 REPEAT 0x86 counts of up to 0x3F; a longer
  # run is written in chunks of 0x40 (`86 FF c`), GCU's loop transcribed (ovl4:0x24ac)
  defp repeat(c, n) when n < 4, do: :binary.copy(<<c>>, n)
  defp repeat(c, n), do: repeat_chunks(c, n, div(n + 0x3E, 0x3F) - 1)

  defp repeat_chunks(c, cx, k) when cx > 0x3F,
    do: <<0x86, 0xFF, c>> <> if(k == 0, do: <<>>, else: repeat_chunks(c, cx - 0x40, k - 1))

  defp repeat_chunks(_c, cx, _k), do: <<0x86, 0xC0 ||| cx>>

  # GCU's text serializer: a colour equal to the last one written (the last graphic colour after a
  # graphic object) is dropped, SI only follows an ESC, the toggles only when they change.
  defp text_blob(w, blob) do
    w =
      case w.prev do
        %{kind: :text} -> w
        nil -> w
        %{} -> %{w | ref_color: if(w.prev_coord, do: w.prev_coord.color, else: <<>>)}
      end

    filter(blob, w)
  end

  defp filter(<<>>, w), do: w

  defp filter(<<0xBE, rest::binary>>, w) do
    {ops, rest} = take_operands_bin(rest, <<>>)
    color = <<0xBE>> <> ops

    if rest == <<>> do
      # a colour still open at the end of the blob is never written
      w
    else
      w = if color == w.ref_color, do: w, else: put(%{w | ref_color: color}, color)
      filter(rest, w)
    end
  end

  defp filter(<<0x1B, rest::binary>>, w), do: filter(rest, put(%{w | esc_seen: true}, <<0x1B>>))

  defp filter(<<0x0F, rest::binary>>, w),
    do: filter(rest, if(w.esc_seen, do: put(%{w | esc_seen: false}, <<0x0F>>), else: w))

  defp filter(<<0x88, rest::binary>>, w), do: filter(rest, toggle(w, :tog88, true, 0x88))
  defp filter(<<0x89, rest::binary>>, w), do: filter(rest, toggle(w, :tog88, false, 0x89))
  defp filter(<<0x99, rest::binary>>, w), do: filter(rest, toggle(w, :tog99, true, 0x99))
  defp filter(<<0x9A, rest::binary>>, w), do: filter(rest, toggle(w, :tog99, false, 0x9A))
  defp filter(<<b, rest::binary>>, w), do: filter(rest, put(w, <<b>>))

  defp toggle(w, key, on, byte) do
    if Map.fetch!(w, key) == on, do: w, else: put(Map.put(w, key, on), <<byte>>)
  end

  defp take_operands_bin(<<b, rest::binary>>, acc) when b >= 0xC0,
    do: take_operands_bin(rest, acc <> <<b>>)

  defp take_operands_bin(rest, acc), do: {acc, rest}

  # ---- output helpers

  # Writing anything clears the last opcode (GCU ovl2:0x836), except the coordinates that go on
  # the command just written.
  defp put(w, bytes, clear \\ true)
  defp put(w, <<>>, _clear), do: w
  defp put(w, bytes, true), do: %{w | out: [bytes | w.out], last_op: 0}
  defp put(w, bytes, false), do: %{w | out: [bytes | w.out]}

  defp put_op(w, op), do: %{w | out: [<<op>> | w.out], last_op: op}

  # ===================================================================== coordinates

  # Coordinates in 1/256 units through NaplpsWriter.mb_xy / XYUtilities.dechunk (multi-value 3).
  defp encode({x, y}), do: NaplpsWriter.mb_xy(<<>>, {x / 256, y / 256})
  defp encode_all(xys), do: xys |> Enum.map(&encode/1) |> IO.iodata_to_binary()

  defp decode_xys(bytes) do
    bin = :binary.list_to_bin(bytes)
    chunks = for <<c::binary-size(3) <- bin>>, do: c

    if rem(byte_size(bin), 3) != 0,
      do: unsupported("coordinate operand not a multiple of 3 bytes")

    chunks
    |> Enum.map(fn chunk ->
      [{x, y}] = chunk |> XYUtilities.dechunk() |> XYUtilities.mul_256()
      {fix_min(round(x), chunk, 5), fix_min(round(y), chunk, 2)}
    end)
  end

  # XYUtilities.dechunk reads -256 (-1.0, sign bit only) as 0; the sign bit tells them apart.
  defp fix_min(0, <<b1, _, _>>, sign_bit) when (b1 >>> sign_bit &&& 1) == 1, do: -256
  defp fix_min(v, _chunk, _sign_bit), do: v

  defp color_index(byte), do: byte >>> 2 &&& 0x0F

  defp fine({x, y}), do: {x * 64, y * 64}
  defp pen(%{cp: {x, y}}), do: {div(x, 64), div(y, 64)}
  defp move_by(%{cp: {x, y}} = d, {dx, dy}), do: %{d | cp: {x + dx * 64, y + dy * 64}}
  defp add_xy({x, y}, {dx, dy}), do: {x + dx, y + dy}
  defp sub_xy({x, y}, {dx, dy}), do: {x - dx, y - dy}
  defp abs_xy({x, y}), do: {abs(x), abs(y)}

  defp hex(bin), do: Base.encode16(bin, case: :lower)
end
