defmodule TelidrawTranspiler.Compiler do
  @moduledoc """
  Walks a parsed Telidraw program and emits NAPLPS bytes through `NaplpsWriter`.

  A faithful port of `NAPLPS.Telidraw.Compiler`, retargeted to the Elixir
  `NaplpsWriter` primitives:

    * geometry (points, lines, rects, arcs, polygons) goes through
      `NaplpsWriter.draw/3`, which appends the opcode and encodes each vertex
      via `NaplpsWriter.mb_xy/2`;
    * `color` uses `NaplpsWriter.select_color/2`;
    * text uses `NaplpsWriter.draw_text_raw/2` / `NaplpsWriter.text_attributes/2`;
    * fixed-byte attribute commands (domain, texture, wait, blink, set-color,
      field, reset) are assembled with `NaplpsWriter.append_bytes/2` and
      `NaplpsWriter.mb_xy/2`.

  `with` blocks compile to explicit-restore (emit new value, walk body, emit the
  previous value). `repeat`/`for`/`if`/`proc` are expanded at compile time.

  Geometry is emitted at multi-byte value 3 (three operand bytes per vertex),
  which is what `NaplpsWriter.mb_xy/2` and `NaplpsWriter.gcu_init/0` assume.
  """

  import Bitwise

  # NAPLPS PDI opcodes (GeneralPDISet base 0xA0), mirroring `NaplpsConstants`
  # and `NAPLPS.NaplpsCommandBuilder`. Passed as the command byte to
  # `NaplpsWriter.draw/3` and friends. (RESET is written by `NaplpsWriter.reset/2`.)
  @op_domain 0xA1
  @op_texture 0xA3
  @op_point_set_abs 0xA4
  @op_point_set_rel 0xA5
  @op_point_abs 0xA6
  @op_point_rel 0xA7
  @op_line_abs 0xA8
  @op_line_rel 0xA9
  @op_set_line_abs 0xAA
  @op_set_line_rel 0xAB
  @op_arc_outlined 0xAC
  @op_arc_filled 0xAD
  @op_set_arc_outlined 0xAE
  @op_set_arc_filled 0xAF
  @op_rect_outlined 0xB0
  @op_rect_filled 0xB1
  @op_set_rect_outlined 0xB2
  @op_set_rect_filled 0xB3
  @op_poly_outlined 0xB4
  @op_poly_filled 0xB5
  @op_set_poly_outlined 0xB6
  @op_set_poly_filled 0xB7
  @op_field 0xB8
  @op_set_color 0xBC
  @op_wait 0xBD
  @op_select_color 0xBE
  @op_blink 0xBF

  # C0 control
  @op_nsr 0x1F

  # Numerical-data base for fixed-format operand bytes (8-bit mode).
  @num_base 0xC0

  # Where RESET with the text bit leaves the cursor and the drawing point: the text home, (0, 190)
  # in 1/256 units whatever TEXT size or FIELD came before (GCU, measured under emulation).
  @text_home {0.0, 190 / 256}

  # Elixir's TEXT fields: proportional spacing (as Prodigy uses), NAPLPS defaults otherwise.
  @default_text_style [spacing: 3, path: 0, rotation: 0, cursor: 0, move: 0, interrow: 0]

  @doc """
  Compile `program` (from `TelidrawTranspiler.Parser`) into `{bytes, diagnostics}`.

  Options: `:init` (default `true`) prepends `NaplpsWriter.gcu_init/0`; `:terminate` (default: the
  value of `:init`) appends GCU's end-of-file `A5` (`NaplpsWriter.gcu_end/1`), so a complete file ends
  the way every GCU-written file does and fragments compiled with `init: false` stay bare.
  """
  @spec compile(map(), keyword()) :: {binary(), [map()]}
  def compile(program, opts \\ []) do
    init = Keyword.get(opts, :init, true)
    terminate = Keyword.get(opts, :terminate, init)
    buffer = if init, do: NaplpsWriter.gcu_init(), else: <<>>

    state = %{
      buffer: buffer,
      pen: {0.0, 0.0},
      vars: %{},
      aliases: %{},
      procs: %{},
      macros: %{},
      next_code: 0x20,
      first_positioned: true,
      # true while compiling a macro or texture definition body (definitions cannot nest)
      defining: false,
      # GCU's text-writer state at the first text object of a save (r25c2e): modes off, esc_seen on.
      text_state: %{reverse_video: false, underline: false, esc_seen: true},
      # The character set in GL, which printable bytes come from (the gcu_init header ends with SI).
      # :primary / :supplementary / :mosaic / :drcs, :macro after an LS3 macro call (text would then
      # play macros), :other when unknown, :body_start at the start of a definition body.
      gl: :primary,
      # The TEXT fields Elixir writes in every TEXT command (codes, NaplpsWriter.text_attributes/3).
      text_style: @default_text_style,
      # DOMAIN logical pel and TEXTURE mask size in effect (the values gcu_init writes), so a `with`
      # block that changes them can put them back.
      pel: {1 / 256, 1 / 256},
      mask_size: {1 / 16, 1 / 16},
      color: 7,
      coord_mode: :fractions,
      dw: 256,
      dh: 192,
      diags: []
    }

    state = Enum.reduce(program.directives, state, &apply_directive/2)

    # Pre-pass: register all proc declarations so procs can call procs declared later.
    state =
      Enum.reduce(program.statements, state, fn
        %{type: :proc_decl} = p, st -> put_in(st.procs[p.name], p)
        _, st -> st
      end)

    state = Enum.reduce(program.statements, state, &compile_statement/2)
    buffer = if terminate, do: NaplpsWriter.gcu_end(state.buffer), else: state.buffer

    {buffer, Enum.reverse(state.diags)}
  end

  # ---- directives ----------------------------------------------------------

  defp apply_directive(%{name: "coord", args: [%{type: :ident, name: name} | _]}, state) do
    case name do
      "fractions" -> %{state | coord_mode: :fractions}
      "pixels" -> %{state | coord_mode: :pixels}
      _ -> state
    end
  end

  defp apply_directive(%{name: "resolution", args: [wa, ha | _]}, state) do
    {w, state} = eval(wa, state)
    {h, state} = eval(ha, state)
    %{state | dw: trunc(w), dh: trunc(h)}
  end

  defp apply_directive(%{name: "bits", args: [ba | _]} = d, state) do
    {bits, state} = eval(ba, state)

    if trunc(bits) == 7 do
      # NaplpsWriter.mb_xy hardcodes the 8-bit (0xC0) numerical base, so 7-bit
      # output is not supported by this emission backend.
      add_diag(state, d, "#bits 7 (7-bit mode) is not supported by NaplpsWriter; emitting 8-bit")
    else
      state
    end
  end

  defp apply_directive(%{name: name} = d, state),
    do: add_diag(state, d, "Unknown or malformed directive '##{name}' (ignored)")

  # ---- statement dispatch --------------------------------------------------

  defp compile_statement(%{type: :command} = c, state), do: compile_command(c, state)
  defp compile_statement(%{type: :proc_call} = p, state), do: compile_proc_call(p, state)
  defp compile_statement(%{type: :with} = w, state), do: compile_with(w, state)
  defp compile_statement(%{type: :repeat} = r, state), do: compile_repeat(r, state)
  defp compile_statement(%{type: :for} = f, state), do: compile_for(f, state)
  defp compile_statement(%{type: :if} = i, state), do: compile_if(i, state)
  defp compile_statement(%{type: :proc_decl, macro: true} = p, state), do: compile_macro_def(p, state)
  defp compile_statement(%{type: :proc_decl}, state), do: state
  defp compile_statement(%{type: :raw} = raw, state), do: compile_raw(raw, state)
  defp compile_statement(%{type: :def_texture} = d, state), do: compile_def_texture(d, state)
  defp compile_statement(%{type: :macro_call} = c, state), do: compile_macro_call(c, state)
  defp compile_statement(%{type: :directive} = d, state), do: apply_directive(d, state)

  defp compile_statement(%{type: :palette_alias} = pa, state) do
    {v, state} = eval(pa.value, state)
    put_in(state.aliases[pa.name], v)
  end

  defp compile_statement(%{type: :let} = l, state) do
    {v, state} = eval(l.value, state)
    put_in(state.vars[l.name], v)
  end

  # ---- command dispatch ----------------------------------------------------

  defp compile_command(%{kind: kind, args: args} = c, state) do
    case kind do
      k when k in [:move, :goto] ->
        with_xy(c, state, fn x, y, st -> emit_move(st, x, y) end)

      :point ->
        with_xy(c, state, fn x, y, st -> emit_point(st, x, y) end)

      :line ->
        emit_polyline(c, state, @op_line_abs, false, "line")

      :rect ->
        with_xy(c, state, fn w, h, st -> emit_rect_filled(st, w, h) end)

      :rect_outline ->
        with_xy(c, state, fn w, h, st ->
          draw(st, @op_rect_outlined, {nx(st, w), ny(st, h)})
        end)

      :move_rel ->
        with_xy(c, state, fn dx, dy, st ->
          draw(st, @op_point_set_rel, {nx(st, dx), ny(st, dy)})
        end)

      :point_rel ->
        with_xy(c, state, fn dx, dy, st ->
          draw(st, @op_point_rel, {nx(st, dx), ny(st, dy)})
        end)

      :line_rel ->
        emit_polyline(c, state, @op_line_rel, true, "line-rel")

      :arc ->
        with_n(c, 4, state, fn [mx, my, ex, ey], st ->
          emit_arc(st, @op_arc_filled, mx, my, ex, ey)
        end)

      :arc_outline ->
        with_n(c, 4, state, fn [mx, my, ex, ey], st ->
          emit_arc(st, @op_arc_outlined, mx, my, ex, ey)
        end)

      :polygon ->
        emit_polygon(c, state, @op_poly_filled)

      :poly_outline ->
        emit_polygon(c, state, @op_poly_outlined)

      :line_set ->
        emit_polyline(c, state, @op_set_line_abs, false, "line-set")

      :line_set_rel ->
        emit_polyline(c, state, @op_set_line_rel, true, "line-set-rel")

      :rect_set ->
        emit_rect_set(c, state, @op_set_rect_filled)

      :rect_set_outline ->
        emit_rect_set(c, state, @op_set_rect_outlined)

      :arc_set ->
        emit_arc_set(c, state, @op_set_arc_filled)

      :arc_set_outline ->
        emit_arc_set(c, state, @op_set_arc_outlined)

      :poly_set ->
        emit_poly_set(c, state, @op_set_poly_filled)

      :poly_set_outline ->
        emit_poly_set(c, state, @op_set_poly_outlined)

      :color ->
        emit_color(c, state)

      :set_color ->
        with_n(c, 3, state, fn [g, r, b], st ->
          emit_bytes(st, set_color_rgb_bytes(trunc(g), trunc(r), trunc(b)))
        end)

      :domain ->
        emit_domain(c, state)

      :texture ->
        emit_texture(c, state)

      :wait ->
        with_n(c, 1, state, fn [t], st ->
          # The fixed byte is 1011100 in bits 7-1 with bit 8 set like every operand byte on an
          # 8-bit link: 0xDC, which is what GCU stores and writes (TITLE.PAG ends `bd dc ca a5`).
          emit_bytes(st, <<@op_wait, 0xDC, interval_byte(trunc(t))>>)
        end)

      :blink ->
        emit_blink(c, state)

      :field ->
        emit_field(c, state)

      :reset ->
        compile_reset(c, state)

      :nsr ->
        emit_bytes(state, <<@op_nsr>>)

      :text ->
        emit_text(c, state)

      :text_raw ->
        emit_text_raw(c, state)

      :text_style ->
        compile_text_style(c, state)

      :close ->
        # No-op: the reference compiler does not track polygon paths.
        state

      other ->
        add_diag(state, c, "Command '#{other}' is not supported (args: #{length(args)})")
    end
  end

  # ---- geometry emission ---------------------------------------------------

  defp emit_move(state, x, y) do
    {nxv, nyv} = {nx(state, x), ny(state, y)}
    state
    |> draw(@op_point_set_abs, {nxv, nyv})
    |> set_pen({nxv, nyv})
    |> Map.put(:first_positioned, false)
  end

  defp emit_point(state, x, y) do
    {nxv, nyv} = {nx(state, x), ny(state, y)}
    state |> draw(@op_point_abs, {nxv, nyv}) |> set_pen({nxv, nyv})
  end

  defp emit_rect_filled(state, w, h) do
    {nwv, nhv} = {nx(state, w), ny(state, h)}
    {px, py} = state.pen
    state |> draw(@op_rect_filled, {nwv, nhv}) |> set_pen({px + nwv, py})
  end

  # Arc operands: (mid relative to pen), (end relative to mid). Pen ends at end.
  defp emit_arc(state, op, mx, my, ex, ey) do
    mxn = nx(state, mx)
    myn = ny(state, my)
    exn = nx(state, ex)
    eyn = ny(state, ey)
    {px, py} = state.pen

    state
    |> draw(op, [{mxn - px, myn - py}, {exn - mxn, eyn - myn}])
    |> set_pen({exn, eyn})
  end

  defp emit_polygon(%{args: args} = c, state, op) do
    if valid_pairs?(args) do
      {nums, state} = eval_all(args, state)
      verts = norm_points(state, nums)
      {rel, last} = abs_to_rel(verts, state.pen)
      state |> draw(op, rel) |> set_pen(last)
    else
      add_diag(state, c, "#{c.kind} needs pairs of x,y coords (got #{length(args)})")
    end
  end

  # Shared emitter for the polyline verbs: `line`, `line-rel`, `line-set`,
  # `line-set-rel`. Each takes one or more x/y pairs and draws the whole
  # connected run in a single command (opcode + N encoded vertices).
  #
  #   * absolute forms encode each vertex absolutely and advance the pen to the
  #     last vertex;
  #   * relative forms encode each pair as a delta and leave the pen unchanged
  #     (deltas are emitted verbatim — the pen tracker is only used as the anchor
  #     for absolute-relative conversions elsewhere).
  defp emit_polyline(%{args: args} = c, state, opcode, relative, verb) do
    if valid_pairs?(args) do
      {nums, state} = eval_all(args, state)
      pts = norm_points(state, nums)

      if relative do
        draw(state, opcode, pts)
      else
        state |> draw(opcode, pts) |> set_pen(List.last(pts))
      end
    else
      add_diag(state, c, "#{verb} needs pairs of x,y coords (got #{length(args)})")
    end
  end

  defp emit_rect_set(c, state, op) do
    with_n(c, 4, state, fn [x, y, w, h], st ->
      draw(st, op, [{nx(st, x), ny(st, y)}, {nx(st, w), ny(st, h)}])
    end)
  end

  # arc-set sx sy mx my ex ey   (all absolute)  or
  # arc-set abs sx sy dmx dmy dex dey   (start absolute + relative deltas)
  defp emit_arc_set(%{args: [%{type: :ident, name: "abs"} | _]} = c, state, op) do
    with_n_from(c, 1, 7, state, fn [sx, sy, dmx, dmy, dex, dey], st ->
      s = {nx(st, sx), ny(st, sy)}
      m = {nx(st, dmx), ny(st, dmy)}
      e = {nx(st, dex), ny(st, dey)}
      {sxn, syn} = s
      {dmxn, dmyn} = m
      {dexn, deyn} = e
      st |> draw(op, [s, m, e]) |> set_pen({sxn + dmxn + dexn, syn + dmyn + deyn})
    end)
  end

  defp emit_arc_set(c, state, op) do
    with_n(c, 6, state, fn [sx, sy, mx, my, ex, ey], st ->
      sxn = nx(st, sx)
      syn = ny(st, sy)
      mxn = nx(st, mx)
      myn = ny(st, my)
      exn = nx(st, ex)
      eyn = ny(st, ey)

      st
      |> draw(op, [{sxn, syn}, {mxn - sxn, myn - syn}, {exn - mxn, eyn - myn}])
      |> set_pen({exn, eyn})
    end)
  end

  # polygon-set sx sy v1x v1y ...        (all absolute) or
  # polygon-set abs sx sy dx1 dy1 ...    (start absolute + relative tail)
  defp emit_poly_set(%{args: [%{type: :ident, name: "abs"} | rest]} = c, state, op) do
    cond do
      length(rest) < 4 or rem(length(rest), 2) != 0 ->
        add_diag(state, c, "#{c.kind} abs needs (sx sy dx1 dy1 ...), got #{length(rest)}")

      true ->
        {nums, state} = eval_all(rest, state)
        [sx, sy | tail] = nums
        start = {nx(state, sx), ny(state, sy)}
        rels = norm_points(state, tail)
        last = Enum.reduce(rels, start, fn {dx, dy}, {px, py} -> {px + dx, py + dy} end)
        state |> draw(op, [start | rels]) |> set_pen(last)
    end
  end

  defp emit_poly_set(%{args: args} = c, state, op) do
    cond do
      length(args) < 4 or rem(length(args), 2) != 0 ->
        add_diag(
          state,
          c,
          "#{c.kind} needs at least 4 coords (start_x start_y v1x v1y ...), got #{length(args)}"
        )

      true ->
        {nums, state} = eval_all(args, state)
        [start | tail_verts] = norm_points(state, nums)
        {rel_tail, _} = abs_to_rel(tail_verts, start)
        last = List.last(tail_verts)
        state |> draw(op, [start | rel_tail]) |> set_pen(last)
    end
  end

  # ---- attribute emission --------------------------------------------------

  defp emit_color(%{args: args} = c, state) do
    case length(args) do
      1 ->
        {[idx], state} = eval_all(args, state)
        idx = trunc(idx)

        %{state | buffer: NaplpsWriter.select_color(state.buffer, select_color_byte(idx))}
        |> Map.put(:color, idx)

      2 ->
        {[fg, bg], state} = eval_all(args, state)
        fg = trunc(fg)
        bg = trunc(bg)
        bytes = <<@op_select_color, select_color_byte(fg), select_color_byte(bg)>>
        emit_bytes(state, bytes) |> Map.put(:color, fg)

      _ ->
        add_diag(state, c, "color takes 1 (fg) or 2 (fg, bg) args")
    end
  end

  # domain sv mv [dim [pelW pelH]]: the fixed byte, then the logical pel size when given (NAP.txt
  # DOMAIN: without it the pel is unchanged; GCU writes the pel whenever it changes).
  defp emit_domain(%{args: args} = c, state) do
    if length(args) < 2 do
      add_diag(state, c, "'domain' needs at least 2 args")
    else
      {nums, state} = eval_all(args, state)

      case domain_cmd(nums, state) do
        {:ok, bytes, pel} -> state |> emit_bytes(bytes) |> put_if(:pel, pel)
        {:error, msg} -> add_diag(state, c, msg)
      end
    end
  end

  # The DOMAIN bytes for `sv mv [dim [pelW pelH]]`, and the pel when one is written. The pel is a
  # multi-value operand at this DOMAIN's own length, so it needs multi-byte 3 and 2-D, the encoding
  # NaplpsWriter.mb_xy produces (other operand lengths are todoelixir step 1).
  defp domain_cmd([sv, mv], _state), do: {:ok, domain_bytes(trunc(sv), trunc(mv), 2), nil}
  defp domain_cmd([sv, mv, dim], _state), do: {:ok, domain_bytes(trunc(sv), trunc(mv), trunc(dim)), nil}

  defp domain_cmd([sv, mv, dim, pw, ph], state) do
    if trunc(mv) == 3 and trunc(dim) != 3 do
      pel = {nx(state, pw), ny(state, ph)}
      {:ok, domain_bytes(trunc(sv), 3, trunc(dim)) <> NaplpsWriter.mb_xy(<<>>, pel), pel}
    else
      {:error, "a domain pel size needs multi-byte 3 and 2-D (the pel is encoded at that length)"}
    end
  end

  defp domain_cmd(_nums, _state), do: {:error, "domain takes sv mv [dim [pelWidth pelHeight]]"}

  # texture line highlight fill [maskW maskH]: the fixed byte, then the size of the programmable
  # masks A-D when given (NAP.txt TEXTURE: without it the mask size is unchanged).
  defp emit_texture(%{args: args} = c, state) do
    {nums, state} = eval_all(args, state)

    case texture_cmd(nums, state) do
      {:ok, bytes, mask} -> state |> emit_bytes(bytes) |> put_if(:mask_size, mask)
      {:error, msg} -> add_diag(state, c, msg)
    end
  end

  defp texture_cmd([line, hl, fill], _state),
    do: {:ok, texture_bytes(trunc(line), hl != 0, trunc(fill)), nil}

  defp texture_cmd([line, hl, fill, mw, mh], state) do
    mask = {nx(state, mw), ny(state, mh)}

    if mw < 0 or mh < 0 do
      # GCU's decoder keeps the sign only as a mirror flag; its TEXTURE record holds the magnitude,
      # so a negative mask size is written back positive (src/naplps_read.c h_texture; measured).
      {:error, "a negative mask size (a mirrored pattern) is not kept by GCU: it writes it back positive"}
    else
      {:ok, texture_bytes(trunc(line), hl != 0, trunc(fill)) <> NaplpsWriter.mb_xy(<<>>, mask), mask}
    end
  end

  defp texture_cmd(_nums, _state),
    do: {:error, "texture takes line highlight fill [maskWidth maskHeight]"}

  defp put_if(state, _key, nil), do: state
  defp put_if(state, key, value), do: Map.put(state, key, value)

  # reset [domain] [color N] [screen N] [text] [blink] [fields] [texture] [macros] [drcs]
  # (NaplpsWriter.reset/2; a bare `reset` resets nothing, as before). The compiler follows what GCU's
  # decoder does (src/naplps_read.c h_reset; the pen measured under emulation): `text` sends the
  # cursor and the drawing point to the text home and turns reverse video and underline off, and
  # Elixir's text style goes back to its default; `macros` deletes every macro, so a later call is
  # an error; `domain` sets the pel back to (0,0). The other bits only reach the file.
  @reset_flags %{
    "domain" => :domain,
    "text" => :text,
    "blink" => :blink,
    "fields" => :fields,
    "texture" => :texture,
    "macros" => :macros,
    "drcs" => :drcs
  }

  defp compile_reset(%{args: args} = c, state) do
    case reset_flags(args, state, []) do
      {:ok, flags, state} -> apply_reset(c, flags, state)
      {:error, msg, state} -> add_diag(state, c, msg)
    end
  end

  defp reset_flags([], state, acc), do: {:ok, Enum.reverse(acc), state}

  defp reset_flags([%{type: :ident, name: n}, value | rest], state, acc)
       when n in ["color", "screen"] do
    {v, state} = eval(value, state)
    v = trunc(v)
    {key, max} = if n == "color", do: {:color, 3}, else: {:screen, 7}

    if v in 0..max,
      do: reset_flags(rest, state, [{key, v} | acc]),
      else: {:error, "reset #{n} must be 0-#{max}, got #{v}", state}
  end

  defp reset_flags([%{type: :ident, name: n} | rest], state, acc)
       when is_map_key(@reset_flags, n),
       do: reset_flags(rest, state, [{Map.fetch!(@reset_flags, n), true} | acc])

  defp reset_flags([%{type: :ident, name: n} | _], state, _acc) when n in ["color", "screen"],
    do: {:error, "reset #{n} needs a number", state}

  defp reset_flags([arg | _], state, _acc) do
    what = if arg[:type] == :ident, do: "'#{arg.name}'", else: "a number"

    {:error,
     "reset takes the words domain, color N, screen N, text, blink, fields, texture, macros, " <>
       "drcs (got #{what})", state}
  end

  defp apply_reset(c, flags, state) do
    if flags[:macros] && state.defining do
      add_diag(state, c, "'reset macros' cannot be used in a macro or texture definition: GCU " <>
        "restarts the definition it is recording")
    else
      state = %{state | buffer: NaplpsWriter.reset(state.buffer, flags)}
      state = if flags[:domain], do: %{state | pel: {0.0, 0.0}}, else: state
      state = if flags[:macros], do: %{state | macros: %{}, next_code: 0x20}, else: state

      if flags[:text] do
        ts = %{state.text_state | reverse_video: false, underline: false}
        %{state | pen: @text_home, text_state: ts, text_style: @default_text_style}
      else
        state
      end
    end
  end

  # text-style [size W H] [spacing S] [path P] [rotation R] [cursor C] [move M] [interrow I]:
  # set the TEXT fields and write a TEXT command (NaplpsWriter.text_attributes/3) at once; without
  # `size` the command has no size operand, so the decoder keeps the size it has. Fields not named
  # keep their current values, and later sized text commands use the same fields.
  @text_fields %{
    "spacing" => {:spacing, %{1 => 0, 1.25 => 1, 1.5 => 2, "proportional" => 3}},
    "path" => {:path, %{"right" => 0, "left" => 1, "up" => 2, "down" => 3}},
    "rotation" => {:rotation, %{0 => 0, 90 => 1, 180 => 2, 270 => 3}},
    "cursor" => {:cursor, %{"underscore" => 0, "block" => 1, "crosshair" => 2, "custom" => 3}},
    "move" =>
      {:move, %{"together" => 0, "cursor-leads" => 1, "point-leads" => 2, "independent" => 3}},
    "interrow" => {:interrow, %{1 => 0, 1.25 => 1, 1.5 => 2, 2 => 3}}
  }

  defp compile_text_style(%{args: args} = c, state) do
    case text_style_args(args, state, state.text_style, nil) do
      {:ok, style, size, state} ->
        buffer = NaplpsWriter.text_attributes(state.buffer, size, style)
        %{state | buffer: buffer, text_style: style}

      {:error, msg, state} ->
        add_diag(state, c, msg)
    end
  end

  defp text_style_args([], state, style, size), do: {:ok, style, size, state}

  defp text_style_args([%{type: :ident, name: "size"}, w, h | rest], state, style, _size) do
    {[wv, hv], state} = eval_all([w, h], state)
    text_style_args(rest, state, style, {wv, hv})
  end

  defp text_style_args([%{type: :ident, name: n}, value | rest], state, style, size)
       when is_map_key(@text_fields, n) do
    {key, codes} = Map.fetch!(@text_fields, n)

    {v, state} =
      case value do
        %{type: :ident, name: word} when is_map_key(codes, word) -> {word, state}
        # a word that is neither a value nor a variable: report the field's values only
        %{type: :ident, name: word} when not is_map_key(state.vars, word) -> {nil, state}
        _ -> eval(value, state)
      end

    case Enum.find(codes, fn {k, _} -> k == v end) do
      {_, code} -> text_style_args(rest, state, Keyword.put(style, key, code), size)
      nil -> {:error, "text-style #{n} takes #{text_field_values(codes)}", state}
    end
  end

  defp text_style_args([arg | _], state, _style, _size) do
    what = if arg[:type] == :ident, do: "'#{arg.name}'", else: "a value"

    {:error,
     "text-style takes size W H, spacing, path, rotation, cursor, move, interrow (got #{what})",
     state}
  end

  defp text_field_values(codes) do
    codes |> Enum.sort_by(&elem(&1, 1)) |> Enum.map_join(", ", fn {k, _} -> to_string(k) end)
  end

  defp emit_blink(%{args: args} = c, state) do
    if length(args) < 3 do
      add_diag(state, c, "blink needs at least (toIndex, onInterval, offInterval)")
    else
      {nums, state} = eval_all(args, state)
      [to_idx, on, off | rest] = nums

      delay =
        case rest do
          [d | _] -> trunc(d)
          [] -> 0
        end

      bytes =
        <<@op_blink, palette_index_byte(trunc(to_idx)), interval_byte(trunc(on)),
          interval_byte(trunc(off)), interval_byte(delay)>>

      emit_bytes(state, bytes)
    end
  end

  defp emit_field(%{args: []}, state), do: emit_bytes(state, <<@op_field>>)

  defp emit_field(%{args: args}, state) when length(args) == 4 do
    {[ox, oy, dx, dy], state} = eval_all(args, state)

    operands =
      NaplpsWriter.mb_xy(<<>>, {nx(state, ox), ny(state, oy)}) <>
        NaplpsWriter.mb_xy(<<>>, {nx(state, dx), ny(state, dy)})

    emit_bytes(state, <<@op_field>> <> operands)
  end

  defp emit_field(c, state),
    do: add_diag(state, c, "field takes 0 (full-screen) or 4 (originX originY dimsX dimsY) args")

  # `text [set] "..." [width height]`. The optional set word picks the character set the string
  # prints from: primary (the default, printable ASCII), supplementary, mosaic or drcs. When GL holds
  # another set, GCU's switch for this one comes first (NaplpsWriter.charset_switch, the bytes GCU's
  # text serializer writes where the set changes). Without a switch after a macro call, the decoder
  # would read the string as macro calls: the call's ESC o leaves the macro set in GL (GCU,
  # measured). The optional size writes a TEXT command first, with the current text-style fields.
  @text_sets %{
    "primary" => :primary,
    "supplementary" => :supplementary,
    "mosaic" => :mosaic,
    "drcs" => :drcs
  }

  defp emit_text(%{args: [%{type: :ident, name: n}, %{type: :str} | _] = [_ | args]} = c, state)
       when is_map_key(@text_sets, n),
       do: emit_text_in(Map.fetch!(@text_sets, n), %{c | args: args}, state)

  defp emit_text(%{args: [%{type: :str} | _]} = c, state), do: emit_text_in(:primary, c, state)

  defp emit_text(c, state),
    do: add_diag(state, c, "text expects a string literal as first arg (after an optional set)")

  defp emit_text_in(set, %{args: [%{type: :str, value: s} | rest]} = c, state) do
    state = text_size(rest, state)
    chars = for <<ch <- s>>, ch >= 0x20 and ch <= text_max(set), into: <<>>, do: <<ch>>

    case bad_text_code(set, chars) do
      nil when chars == <<>> ->
        state

      nil ->
        state = switch_gl(state, set)
        %{state | buffer: NaplpsWriter.draw_text_raw(state.buffer, chars)}

      msg ->
        add_diag(state, c, msg)
    end
  end

  # Highest printable code per set: DEL (0x7F) is a character in the mosaic and DRCS sets (GCU keeps
  # it), not in the primary and supplementary sets (GCU's decoder skips it there).
  defp text_max(set) when set in [:mosaic, :drcs], do: 0x7F
  defp text_max(_set), do: 0x7E

  # Codes GCU does not keep, measured under emulation: mosaic 0x40-0x5E print as spaces (GCU stores
  # 0x20, src/naplps_read.c set_mosaic); supplementary 0x40-0x4F are non-spacing accents, which GCU
  # writes back as primary-set letters, losing the accent.
  defp bad_text_code(:mosaic, chars) do
    if Enum.any?(:binary.bin_to_list(chars), &(&1 in 0x40..0x5E)),
      do: "mosaic codes 0x40-0x5E are not mosaic characters (GCU prints them as spaces)"
  end

  defp bad_text_code(:supplementary, chars) do
    if Enum.any?(:binary.bin_to_list(chars), &(&1 in 0x40..0x4F)),
      do:
        "supplementary codes 0x40-0x4F are non-spacing accents, which GCU does not keep " <>
          "(it writes them back as primary-set letters)"
  end

  defp bad_text_code(_set, _chars), do: nil

  # The optional [width height] of a text command: a TEXT command sizing the glyphs, with the
  # current text-style fields.
  defp text_size(rest, state) when length(rest) >= 2 do
    {[w, h], state} = eval_all(Enum.take(rest, 2), state)
    %{state | buffer: NaplpsWriter.text_attributes(state.buffer, {w, h}, state.text_style)}
  end

  defp text_size(_rest, state), do: state

  # Put `set` into GL with GCU's switch when it is not there. A switch that ends in SI leaves the
  # writer's esc_seen off; the others end an escape sequence (NaplpsWriter.serialize_text).
  defp switch_gl(%{gl: set} = state, set), do: state

  defp switch_gl(state, set) do
    %{
      state
      | buffer: NaplpsWriter.charset_switch(state.buffer, set),
        gl: set,
        text_state: %{state.text_state | esc_seen: set != :primary}
    }
  end

  # `text-raw "..." [width height]`: like `text`, but the string keeps its C0/C1 control codes
  # (authored via \xHH escapes) and is serialized the way GCU's writer does — NULs dropped,
  # redundant reverse-video/underline/SI toggles suppressed (NaplpsWriter.serialize_text). The
  # toggle state persists across text commands (GCU keeps it writer-global). No pen move. The bytes
  # are written as given, except that an SI goes first when GL holds the macro set (after a macro
  # call, or at the start of a definition body), where printable bytes would play macros.
  defp emit_text_raw(%{args: [%{type: :str, value: s} | rest]}, state) do
    state = text_size(rest, state)
    state = if state.gl in [:macro, :body_start], do: switch_gl(state, :primary), else: state
    {filtered, tstate} = NaplpsWriter.serialize_text(s, state.text_state)

    %{
      state
      | buffer: NaplpsWriter.draw_text_raw(state.buffer, filtered),
        text_state: tstate,
        gl: gl_after(filtered, state.gl)
    }
  end

  defp emit_text_raw(c, state),
    do: add_diag(state, c, "text-raw expects a string literal as first arg")

  # The set in GL after `bytes`: SI -> primary, ESC n (LS2) -> supplementary, ESC o (LS3) after
  # GCU's mosaic / DRCS / macro designation -> that set; any other LS3 or SO, or a G3 designation
  # while a G3 set is in GL, -> unknown. SS2/SS3 shift one character only and leave GL alone.
  defp gl_after(<<>>, gl), do: gl
  defp gl_after(<<0x0F, rest::binary>>, _gl), do: gl_after(rest, :primary)
  defp gl_after(<<0x0E, rest::binary>>, _gl), do: gl_after(rest, :other)
  defp gl_after(<<0x1B, 0x6E, rest::binary>>, _gl), do: gl_after(rest, :supplementary)
  defp gl_after(<<0x1B, 0x2F, 0x7D, 0x1B, 0x6F, rest::binary>>, _gl), do: gl_after(rest, :mosaic)
  defp gl_after(<<0x1B, 0x2F, 0x7B, 0x1B, 0x6F, rest::binary>>, _gl), do: gl_after(rest, :drcs)
  defp gl_after(<<0x1B, 0x2F, 0x20, 0x7B, 0x1B, 0x6F, rest::binary>>, _gl), do: gl_after(rest, :drcs)
  defp gl_after(<<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, rest::binary>>, _gl), do: gl_after(rest, :macro)
  defp gl_after(<<0x1B, 0x6F, rest::binary>>, _gl), do: gl_after(rest, :other)

  defp gl_after(<<0x1B, 0x2F, rest::binary>>, gl) when gl in [:mosaic, :drcs, :macro],
    do: gl_after(rest, :other)

  defp gl_after(<<_, rest::binary>>, gl), do: gl_after(rest, gl)

  # ---- with / repeat / for / if / proc -------------------------------------

  defp compile_with(%{attr: :color, attr_args: []} = w, state),
    do: add_diag(state, w, "with color needs a color index or alias")

  defp compile_with(%{attr: :color, attr_args: [ca | _], body: body}, state) do
    prev = state.color
    {new_color, state} = eval(ca, state)
    new_color = trunc(new_color)

    state =
      %{state | buffer: NaplpsWriter.select_color(state.buffer, select_color_byte(new_color))}
      |> Map.put(:color, new_color)

    state = Enum.reduce(body, state, &compile_statement/2)

    %{state | buffer: NaplpsWriter.select_color(state.buffer, select_color_byte(prev))}
    |> Map.put(:color, prev)
  end

  defp compile_with(%{attr: :texture, attr_args: aargs, body: body} = w, state) do
    {nums, state} = eval_all(aargs, state)

    case texture_cmd(nums, state) do
      {:ok, bytes, mask} ->
        prev = state.mask_size
        state = state |> emit_bytes(bytes) |> put_if(:mask_size, mask)
        state = Enum.reduce(body, state, &compile_statement/2)
        # Restore the default texture (solid line, no highlight, solid fill) and, when the block
        # set a mask size, the one in effect before it.
        restore = if mask, do: NaplpsWriter.mb_xy(<<>>, prev), else: <<>>
        %{emit_bytes(state, texture_bytes(0, false, 0) <> restore) | mask_size: prev}

      {:error, msg} ->
        add_diag(state, w, "with " <> msg)
    end
  end

  defp compile_with(%{attr: :domain, attr_args: aargs, body: body} = w, state) do
    {nums, state} = eval_all(aargs, state)

    case domain_cmd(nums, state) do
      {:ok, bytes, pel} ->
        prev = state.pel
        state = state |> emit_bytes(bytes) |> put_if(:pel, pel)
        state = Enum.reduce(body, state, &compile_statement/2)
        # Restore defaults (single-byte 1, multi-byte 3, 2D) and, when the block set a pel, the
        # pel in effect before it.
        restore = if pel, do: NaplpsWriter.mb_xy(<<>>, prev), else: <<>>
        %{emit_bytes(state, domain_bytes(1, 3, 2) <> restore) | pel: prev}

      {:error, msg} ->
        add_diag(state, w, "with " <> msg)
    end
  end

  defp compile_with(w, state),
    do: add_diag(state, w, "'with #{w.attr}' is not supported")

  defp compile_repeat(%{count: count, body: body}, state) do
    {n, state} = eval(count, state)
    n = trunc(n)

    if n <= 0 do
      state
    else
      Enum.reduce(1..n, state, fn _, st -> Enum.reduce(body, st, &compile_statement/2) end)
    end
  end

  defp compile_for(%{var: var, from: from_e, to: to_e, body: body}, state) do
    {from, state} = eval(from_e, state)
    {to, state} = eval(to_e, state)
    from = trunc(from)
    to = trunc(to)

    had = Map.has_key?(state.vars, var)
    prev = Map.get(state.vars, var)

    state =
      if from > to do
        state
      else
        Enum.reduce(from..to, state, fn i, st ->
          st = put_in(st.vars[var], i)
          Enum.reduce(body, st, &compile_statement/2)
        end)
      end

    vars = if had, do: Map.put(state.vars, var, prev), else: Map.delete(state.vars, var)
    %{state | vars: vars}
  end

  defp compile_if(%{cond: cond_e, then: then_body, else: else_body}, state) do
    {c, state} = eval(cond_e, state)
    body = if c != 0, do: then_body, else: else_body

    case body do
      nil -> state
      stmts -> Enum.reduce(stmts, state, &compile_statement/2)
    end
  end

  defp compile_proc_call(%{name: name} = call, state) do
    case Map.fetch(state.procs, name) do
      :error ->
        add_diag(state, call, "Unknown procedure '#{name}'")

      {:ok, proc} ->
        if length(call.args) != length(proc.params) do
          add_diag(
            state,
            call,
            "'#{name}' expects #{length(proc.params)} args, got #{length(call.args)}"
          )
        else
          {argvals, state} = eval_all(call.args, state)
          params = proc.params
          shadowed = Map.take(state.vars, params)
          bindings = params |> Enum.zip(argvals) |> Map.new()

          state = %{state | vars: Map.merge(state.vars, bindings)}
          state = Enum.reduce(proc.body, state, &compile_statement/2)

          restored = state.vars |> Map.drop(params) |> Map.merge(shadowed)
          %{state | vars: restored}
        end
    end
  end

  # ---- raw -----------------------------------------------------------------
  #
  # Raw statements come from two sources: the literal `raw <opcode> <bytes...>`
  # form (`logical: false`) and the UPPERCASE mnemonic form `DOMAIN 200 192 ...`
  # (`logical: true`). Both emit the opcode + operand bytes verbatim; this backend
  # only produces 8-bit output, so no bit-7 stripping is applied. The pen position
  # is mirrored for position-changing PDI opcodes so any following high-level
  # command computes its relative deltas from the right anchor.

  defp compile_raw(%{bytes: []}, state), do: state

  defp compile_raw(%{bytes: [opcode | operands]}, state) do
    state
    |> emit_bytes(:binary.list_to_bin([opcode | operands]))
    |> update_pen_from_raw(opcode, operands)
  end

  # Mirror the pen-end side effect of a raw-emitted PDI command (mv = 3, matching
  # NaplpsWriter.mb_xy). Only opcodes whose normal constructor moves the pen are
  # handled; everything else is a no-op. Opcode bit 7 is stripped first so 7-bit
  # (0x20-0x7F) and 8-bit (0xA0-0xFF) presentations normalize to the same case.
  defp update_pen_from_raw(state, opcode, operands),
    do: set_pen(state, pen_after_pdi(state.pen, opcode, operands))

  # Where the drawing point ends after one PDI command started at `pen` (pure, so a macro body can
  # be replayed through the same rules -- replay_pen/4).
  defp pen_after_pdi({px, py} = pen, opcode, operands) do
    verts = decode_vertices(operands)

    case opcode &&& 0x7F do
      # RESET with the text bit (second operand byte, bit 1) sends the drawing point to the text
      # home (GCU, measured); no other RESET bit moves it.
      0x20 ->
        case operands do
          [_, b2 | _] when (b2 &&& 0x01) == 1 -> @text_home
          _ -> pen
        end

      # Absolute pen sets — pen lands exactly at the (last) decoded vertex.
      op when op in [0x24, 0x26, 0x28] and verts != [] ->
        List.last(verts)

      # LineSetAbsolute — pen ends at the last absolute vertex.
      0x2A when verts != [] ->
        List.last(verts)

      # Relative pen offsets / cumulative-relative walks — pen advances by the sum.
      op when op in [0x25, 0x27, 0x29, 0x2B, 0x34, 0x35, 0x3A, 0x3B] ->
        sum_deltas(pen, verts)

      # Arc (mid then end, both relative to pen) — pen ends at the end vertex.
      op when op in [0x2C, 0x2D] and length(verts) >= 2 ->
        [{mdx, mdy}, {edx, edy} | _] = verts
        {px + mdx + edx, py + mdy + edy}

      # ArcSet (start absolute, mid + end relative) — pen ends at start+dm+de.
      op when op in [0x2E, 0x2F] and length(verts) >= 3 ->
        [{sx, sy}, {dmx, dmy}, {dex, dey} | _] = verts
        {sx + dmx + dex, sy + dmy + dey}

      # Rect runs (w,h pairs) — GCU advances the pen by each width (SUPPORTED.md 30-33), the
      # same rule emit_rect_filled applies.
      op when op in [0x30, 0x31] ->
        Enum.reduce(verts, pen, fn {w, _h}, {x, y} -> {x + w, y} end)

      # SetRect (start absolute, then w,h pairs) — pen = start, advanced by each width.
      op when op in [0x32, 0x33] and verts != [] ->
        [start | sizes] = verts
        Enum.reduce(sizes, start, fn {w, _h}, {x, y} -> {x + w, y} end)

      # PolygonSet (first vertex absolute, rest relative) — pen ends at the sum.
      op when op in [0x36, 0x37] and length(verts) >= 2 ->
        [start | rels] = verts
        sum_deltas(start, rels)

      _ ->
        pen
    end
  end

  # Where a decoder's drawing point ends after playing `bytes` (a macro's recording, see
  # record_body/4) from `pen`. PDI commands (opcode 0xA0-0xBF plus its 0xC0-0xFF operand bytes)
  # go through pen_after_pdi; a call (ESC / z then ESC o or SS3, then the code) plays that macro's
  # recording. Depth-limited: GCU hangs on a macro that calls itself while it plays. Text is not
  # modelled (the top-level text commands do not move the pen either).
  defp replay_pen(bytes, pen, recordings, depth \\ 0)
  defp replay_pen(<<>>, pen, _recordings, _depth), do: pen

  defp replay_pen(<<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, code, rest::binary>>, pen, recs, depth),
    do: replay_pen(rest, replay_call(code, pen, recs, depth), recs, depth)

  defp replay_pen(<<0x1B, 0x2F, 0x7A, 0x1D, code, rest::binary>>, pen, recs, depth),
    do: replay_pen(rest, replay_call(code, pen, recs, depth), recs, depth)

  defp replay_pen(<<op, rest::binary>>, pen, recs, depth) when op in 0xA0..0xBF do
    {operands, rest} = take_operands(rest, [])
    replay_pen(rest, pen_after_pdi(pen, op, operands), recs, depth)
  end

  defp replay_pen(<<_, rest::binary>>, pen, recs, depth), do: replay_pen(rest, pen, recs, depth)

  defp replay_call(code, pen, recs, depth) when depth < 16 do
    case Map.fetch(recs, code) do
      {:ok, recording} -> replay_pen(recording, pen, recs, depth + 1)
      :error -> pen
    end
  end

  defp replay_call(_code, pen, _recs, _depth), do: pen

  defp take_operands(<<b, rest::binary>>, acc) when b in 0xC0..0xFF, do: take_operands(rest, [b | acc])
  defp take_operands(rest, acc), do: {Enum.reverse(acc), rest}

  # What GCU stores for a macro whose definition body is `bytes` (measured: the C reader's macro
  # store, byte-exact with GCU's). GCU decodes a definition while it records it, so a call inside
  # the body plays the called macro, and the bytes that playback feeds are recorded too: right
  # after the call's code byte comes the called macro's own recording, expanded the same way.
  # Playing the outer macro later therefore runs the inner one twice (via the call, then from the
  # copy). The macro being defined is not played, nor is an undefined one. Recording stops at a
  # definition opcode (C1 0x80-0x85), which ends the definition in GCU.
  defp record_body(bytes, recs, self_code, depth \\ 0)
  defp record_body(<<>>, _recs, _self, _depth), do: <<>>
  defp record_body(<<op, _::binary>>, _recs, _self, _depth) when op in 0x80..0x85, do: <<>>

  defp record_body(<<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, code, rest::binary>>, recs, self, depth),
    do: <<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, code>> <> played(code, recs, self, depth) <>
          record_body(rest, recs, self, depth)

  defp record_body(<<0x1B, 0x2F, 0x7A, 0x1D, code, rest::binary>>, recs, self, depth),
    do: <<0x1B, 0x2F, 0x7A, 0x1D, code>> <> played(code, recs, self, depth) <>
          record_body(rest, recs, self, depth)

  defp record_body(<<b, rest::binary>>, recs, self, depth),
    do: <<b>> <> record_body(rest, recs, self, depth)

  defp played(code, _recs, code, _depth), do: <<>>

  defp played(code, recs, self, depth) when depth < 16 do
    case Map.fetch(recs, code) do
      {:ok, recording} -> record_body(recording, recs, self, depth + 1)
      :error -> <<>>
    end
  end

  defp played(_code, _recs, _self, _depth), do: <<>>

  defp recordings(state), do: Map.new(state.macros, fn {_name, m} -> {m.code, m.recorded} end)

  defp sum_deltas(origin, deltas),
    do: Enum.reduce(deltas, origin, fn {dx, dy}, {x, y} -> {x + dx, y + dy} end)

  # ---- DEF MACRO -------------------------------------------------------------

  # `@macro proc name(params) [code] { body }`: a macro definition. The body is compiled into its
  # own buffer through the normal emitters (the re-encoded drawing commands, via draw/mb_xy — not
  # the recorded source), then framed as `80 <code> body A5 85` via NaplpsWriter.def_macro. GCU
  # saves DEFP/DEFT as a plain 0x80 too. A body that draws nothing makes no object (skip emission),
  # but the macro is still registered so a call resolves its code (invocation is step 4). Params are
  # bound to 0 for the definition's concrete geometry; the code is explicit or auto-assigned from
  # 0x20 (96 slots, 0x20-0x7F). Besides the body, the macro keeps GCU's recording of it
  # (record_body/4: the body + the def_macro framing's A5, with nested calls expanded the way GCU
  # records them); a later call replays the recording to find where the pen ends. A definition
  # cannot be nested inside another one: GCU ends the outer definition at the inner one's opcode.
  defp compile_macro_def(%{name: name} = p, %{defining: true} = state),
    do: add_diag(state, p, nested_definition_message("'@macro proc #{name}'"))

  defp compile_macro_def(%{name: name, params: params, body: body, code: code_e} = p, state) do
    {code, state} = macro_code(code_e, state)
    bindings = Map.new(params, fn pn -> {pn, 0} end)

    sub = %{
      state
      | buffer: <<>>,
        pen: {0.0, 0.0},
        vars: Map.merge(state.vars, bindings),
        diags: [],
        defining: true,
        gl: :body_start
    }

    sub = Enum.reduce(body, sub, &compile_statement/2)
    # GCU decodes the body while it records it, so the body's text and calls change GL then.
    state = %{state | diags: sub.diags ++ state.diags, gl: gl_after_body(sub.gl, state.gl)}

    cond do
      code == nil ->
        add_diag(state, p, "macro code must be 0x20-0x7F (32-127)")

      sub.buffer == <<>> ->
        # GCU: a body that draws nothing makes no DEF MACRO object; still register the name.
        macro = %{code: code, body: <<>>, recorded: <<>>}
        %{state | macros: Map.put(state.macros, name, macro), next_code: code + 1}

      true ->
        recorded = record_body(sub.buffer <> <<0xA5>>, recordings(state), code)
        macro = %{code: code, body: sub.buffer, recorded: recorded}

        %{
          state
          | buffer: NaplpsWriter.def_macro(state.buffer, code, sub.buffer),
            macros: Map.put(state.macros, name, macro),
            next_code: code + 1
        }
    end
  end

  defp nested_definition_message(what),
    do:
      "#{what} cannot be nested inside a macro or texture definition: GCU ends the outer " <>
        "definition at the inner one's opcode"

  # A body starts with GL unknown (:body_start: when it plays, GL may hold the macro set, so its
  # first text begins with an SI). A body that changed GL leaves its set in GL.
  defp gl_after_body(:body_start, outer), do: outer
  defp gl_after_body(body_gl, _outer), do: body_gl

  defp macro_code(nil, state), do: {state.next_code, state}

  defp macro_code(expr, state) do
    {v, state} = eval(expr, state)
    c = trunc(v)
    if c in 0x20..0x7F, do: {c, state}, else: {nil, state}
  end

  # `@name args`: invoke a defined macro at the current pen. GCU positions the first positioned
  # object absolutely (A4) and later ones pen-relative (A5); the call object's blob is the code
  # byte, framed by the invocation escape + code-set restore (NaplpsWriter.macro_call). Args are
  # parsed but do not change the invocation: GCU plays the recorded body regardless of call args.
  # first_positioned is cleared by move and by a macro call (full tracking across every object
  # type is a refinement; the common move-then-call and call-first cases are covered). Playing the
  # body moves the drawing point (GCU, measured: a LINE REL body moves it by the delta), so the
  # pen becomes wherever the body leaves it when played from the call position (replay_pen).
  # `@name ss3` uses GCU's SS3 form (escape record 8, which GCU writes for a call it read that way);
  # the default LS3 form leaves the macro set in GL, so the next text starts with an SI (switch_gl).
  defp compile_macro_call(%{name: name} = c, state) do
    case Map.fetch(state.macros, name) do
      :error ->
        add_diag(state, c, "Unknown macro '#{name}' (define it with @macro proc)")

      {:ok, %{code: code, recorded: recorded}} ->
        {px, py} = state.pen
        ss3 = Map.get(c, :ss3, false)
        # GCU: first positioned object -> A4 absolute (the call position = the pen); later ->
        # A5 pen-relative delta. The call is invoked at the current pen, so the delta is (0,0).
        {pos_op, operand} =
          if state.first_positioned, do: {0xA4, {px, py}}, else: {0xA5, {0.0, 0.0}}

        %{
          state
          | buffer: NaplpsWriter.macro_call(state.buffer, code, operand, pos_op: pos_op, ss3: ss3),
            pen: replay_pen(recorded, {px, py}, recordings(state)),
            first_positioned: false,
            gl: call_gl(ss3, state.gl),
            text_state: %{state.text_state | esc_seen: true}
        }
    end
  end

  # GL after a call: LS3 puts G3, now the macro set, in GL. SS3 shifts only the code byte, but the
  # call's ESC / z still re-designates G3, which changes GL when GL holds G3 (mosaic or DRCS text).
  defp call_gl(false, _gl), do: :macro
  defp call_gl(true, gl) when gl in [:mosaic, :drcs], do: :macro
  defp call_gl(true, gl), do: gl

  # ---- DEF TEXTURE -----------------------------------------------------------

  # `def-texture <mask> { body }`: compile the body block into its own buffer through the
  # normal emitters (so its geometry goes through NaplpsWriter.draw/mb_xy), then frame it as
  # `84 <mask> body 85` + the code-set restore via NaplpsWriter.def_texture. The body is raw
  # (GCU does not decode it). When the definition ends, GCU's decoder puts the drawing point back
  # at (0,0) whatever the pen was (measured under emulation; NAP.txt says the same), so the pen
  # becomes (0,0) here too. Like a macro, it cannot be nested inside another definition.
  defp compile_def_texture(d, %{defining: true} = state),
    do: add_diag(state, d, nested_definition_message("'def-texture'"))

  defp compile_def_texture(%{mask: mask_e, body: body} = d, state) do
    {mask_v, state} = eval(mask_e, state)
    sub = %{state | buffer: <<>>, pen: {0.0, 0.0}, diags: [], defining: true, gl: :body_start}
    sub = Enum.reduce(body, sub, &compile_statement/2)
    # Whether GCU decodes text in a mask body is not known, so text there makes GL unknown.
    gl = if sub.gl == :body_start, do: state.gl, else: :other
    state = %{state | diags: sub.diags ++ state.diags, gl: gl}

    case mask_byte(trunc(mask_v)) do
      nil ->
        add_diag(state, d, "def-texture mask must be 1-4 (masks A-D), got #{trunc(mask_v)}")

      mb ->
        %{state | buffer: NaplpsWriter.def_texture(state.buffer, mb, sub.buffer), pen: {0.0, 0.0}}
    end
  end

  # Semantic mask 1-4 -> the operand byte 'A'-'D' (0x41-0x44); a literal A-D byte also accepted.
  defp mask_byte(m) when m in 1..4, do: 0x40 + m
  defp mask_byte(m) when m in 0x41..0x44, do: m
  defp mask_byte(_), do: nil

  # ---- expression evaluator ------------------------------------------------

  defp eval(%{type: :num, value: v}, state), do: {v, state}
  defp eval(%{type: :frac, num: n, den: d}, state), do: {n / d, state}

  defp eval(%{type: :str} = e, state),
    do: {0, add_diag(state, e, "Cannot use a string in a numeric context")}

  defp eval(%{type: :ident, name: name} = e, state) do
    cond do
      Map.has_key?(state.vars, name) -> {Map.fetch!(state.vars, name), state}
      Map.has_key?(state.aliases, name) -> {Map.fetch!(state.aliases, name), state}
      true -> {0, add_diag(state, e, "Unknown identifier '#{name}'")}
    end
  end

  defp eval(%{type: :unop, op: op, operand: operand}, state) do
    {v, state} = eval(operand, state)
    {if(op == "-", do: -v, else: v), state}
  end

  defp eval(%{type: :binop, op: op, left: l, right: r}, state) do
    {lv, state} = eval(l, state)
    {rv, state} = eval(r, state)

    value =
      case op do
        "+" -> lv + rv
        "-" -> lv - rv
        "*" -> lv * rv
        "/" -> if rv == 0, do: 0, else: lv / rv
        "%" -> if rv == 0, do: 0, else: safe_rem(lv, rv)
        _ -> 0
      end

    {value, state}
  end

  defp eval(%{type: :call} = e, state),
    do: {0, add_diag(state, e, "Function-call expressions are not evaluated at compile time")}

  defp eval_all(exprs, state) do
    {rev, state} =
      Enum.reduce(exprs, {[], state}, fn e, {acc, st} ->
        {v, st} = eval(e, st)
        {[v | acc], st}
      end)

    {Enum.reverse(rev), state}
  end

  # ---- argument helpers ----------------------------------------------------

  # Evaluate exactly-2 args (x, y) and hand them to `fun`, or diagnose.
  defp with_xy(%{args: args} = c, state, fun) do
    if length(args) < 2 do
      add_diag(state, c, "'#{c.kind}' needs 2 args, got #{length(args)}")
    else
      {[a, b | _], state} = eval_all(args, state)
      fun.(a, b, state)
    end
  end

  # Evaluate the first `n` args and hand the list to `fun`, or diagnose.
  defp with_n(%{args: args} = c, n, state, fun) do
    if length(args) < n do
      add_diag(state, c, "'#{c.kind}' needs at least #{n} args, got #{length(args)}")
    else
      {vals, state} = eval_all(Enum.take(args, n), state)
      fun.(vals, state)
    end
  end

  # Like `with_n` but skips the leading `drop` args (e.g. the `abs` marker).
  defp with_n_from(%{args: args} = c, drop, total, state, fun) do
    if length(args) < total do
      add_diag(state, c, "'#{c.kind}' needs #{total} args, got #{length(args)}")
    else
      {vals, state} = eval_all(args |> Enum.drop(drop) |> Enum.take(total - drop), state)
      fun.(vals, state)
    end
  end

  # ---- byte builders -------------------------------------------------------

  defp domain_bytes(sv, mv, dim) do
    data =
      (clamp(sv, 1, 4) - 1 &&& 0x03) |||
        (clamp(mv, 1, 8) - 1 &&& 0x07) <<< 2 |||
        if(dim == 3, do: 1, else: 0) <<< 5

    <<@op_domain, @num_base ||| data>>
  end

  defp texture_bytes(line, highlight, fill) do
    data =
      (line &&& 0x03) |||
        if(highlight, do: 1, else: 0) <<< 2 |||
        (fill &&& 0x07) <<< 3

    <<@op_texture, @num_base ||| data>>
  end

  # Single-byte RGB (color mode 0). Mirrors NaplpsCommandBuilder.BuildSetColorRgb
  # with byteCount=1: 2 bits per component, packed G,R,B high then G,R,B low.
  defp set_color_rgb_bytes(g, r, b) do
    max = 3
    gv = div(g * max, 255)
    rv = div(r * max, 255)
    bv = div(b * max, 255)

    data =
      (gv >>> 1 &&& 1) <<< 5 |||
        (rv >>> 1 &&& 1) <<< 4 |||
        (bv >>> 1 &&& 1) <<< 3 |||
        (gv &&& 1) <<< 2 |||
        (rv &&& 1) <<< 1 |||
        (bv &&& 1)

    <<@op_set_color, @num_base ||| data>>
  end

  defp select_color_byte(index), do: @num_base ||| (index &&& 0x0F) <<< 2
  defp palette_index_byte(index), do: @num_base ||| (index &&& 0x0F) <<< 2
  defp interval_byte(value), do: @num_base ||| (value &&& 0x3F)

  # ---- low-level state helpers ---------------------------------------------

  defp draw(state, op, points),
    do: %{state | buffer: NaplpsWriter.draw(state.buffer, op, points)}

  defp emit_bytes(state, bytes),
    do: %{state | buffer: NaplpsWriter.append_bytes(state.buffer, bytes)}

  defp set_pen(state, {_x, _y} = pen), do: %{state | pen: pen}

  defp nx(%{coord_mode: :fractions}, v), do: v
  defp nx(%{coord_mode: :pixels, dw: dw}, v), do: v / dw

  defp ny(%{coord_mode: :fractions}, v), do: v
  defp ny(%{coord_mode: :pixels, dh: dh}, v), do: v / dh

  defp norm_points(state, nums) do
    nums
    |> Enum.chunk_every(2)
    |> Enum.map(fn [x, y] -> {nx(state, x), ny(state, y)} end)
  end

  # Decode raw operand bytes back into {x, y} fractions, the inverse of
  # NaplpsWriter.mb_xy at mv = 3 (three bytes per vertex; x bits at positions
  # 5,4,3 and y bits at 2,1,0 of each byte, ignoring the high numerical-data bits).
  defp decode_vertices(bytes) do
    bytes
    |> Enum.chunk_every(3, 3, :discard)
    |> Enum.map(&decode_vertex/1)
  end

  defp decode_vertex([b1, b2, b3]) do
    xbits = for b <- [b1, b2, b3], bit <- [5, 4, 3], do: b >>> bit &&& 1
    ybits = for b <- [b1, b2, b3], bit <- [2, 1, 0], do: b >>> bit &&& 1
    {bits_to_fraction(xbits), bits_to_fraction(ybits)}
  end

  # First bit is the sign; the remaining 8 are the binary fraction. Negatives are
  # stored two's-complement (matching NaplpsWriter.make_bits).
  defp bits_to_fraction([sign | rest]) do
    mag = Enum.reduce(rest, 0, fn b, acc -> acc * 2 + b end)

    if sign == 1 do
      comp = bnot(mag) + 1 &&& 0xFF
      -comp / 256
    else
      mag / 256
    end
  end

  defp abs_to_rel(verts, pen) do
    {rev, last} =
      Enum.reduce(verts, {[], pen}, fn {x, y}, {acc, {px, py}} ->
        {[{x - px, y - py} | acc], {x, y}}
      end)

    {Enum.reverse(rev), last}
  end

  defp valid_pairs?(args), do: length(args) >= 2 and rem(length(args), 2) == 0

  defp clamp(v, lo, hi), do: v |> max(lo) |> min(hi)

  # Integer remainder when both sides are whole; float otherwise (matches C# `%`).
  defp safe_rem(a, b) when is_integer(a) and is_integer(b), do: rem(a, b)
  defp safe_rem(a, b), do: a - trunc(a / b) * b

  defp add_diag(state, node, msg) do
    diag = %{
      severity: :error,
      line: Map.get(node, :line, 0),
      column: Map.get(node, :col, 0),
      message: msg
    }

    %{state | diags: [diag | state.diags]}
  end
end
