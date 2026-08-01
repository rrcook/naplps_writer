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
  # `NaplpsWriter.draw/3` and friends.
  @op_reset 0xA0
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

  @doc """
  Compile `program` (from `TelidrawTranspiler.Parser`) into `{bytes, diagnostics}`.

  Options: `:init` (default `true`) prepends `NaplpsWriter.gcu_init/0`.
  """
  @spec compile(map(), keyword()) :: {binary(), [map()]}
  def compile(program, opts \\ []) do
    init = Keyword.get(opts, :init, true)
    buffer = if init, do: NaplpsWriter.gcu_init(), else: <<>>

    state = %{
      buffer: buffer,
      pen: {0.0, 0.0},
      vars: %{},
      aliases: %{},
      procs: %{},
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

    {state.buffer, Enum.reverse(state.diags)}
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
  defp compile_statement(%{type: :proc_decl}, state), do: state
  defp compile_statement(%{type: :raw} = raw, state), do: compile_raw(raw, state)
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
        with_xy(c, state, fn x, y, st -> emit_line(st, x, y) end)

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
        with_xy(c, state, fn dx, dy, st ->
          draw(st, @op_line_rel, {nx(st, dx), ny(st, dy)})
        end)

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
        emit_line_set(c, state, false)

      :line_set_rel ->
        emit_line_set(c, state, true)

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
        with_n(c, 3, state, fn [line, hl, fill], st ->
          emit_bytes(st, texture_bytes(trunc(line), hl != 0, trunc(fill)))
        end)

      :wait ->
        with_n(c, 1, state, fn [t], st ->
          emit_bytes(st, <<@op_wait, 0x5C, interval_byte(trunc(t))>>)
        end)

      :blink ->
        emit_blink(c, state)

      :field ->
        emit_field(c, state)

      :reset ->
        emit_bytes(state, <<@op_reset, @num_base, @num_base>>)

      :nsr ->
        emit_bytes(state, <<@op_nsr>>)

      :text ->
        emit_text(c, state)

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
    state |> draw(@op_point_set_abs, {nxv, nyv}) |> set_pen({nxv, nyv})
  end

  defp emit_point(state, x, y) do
    {nxv, nyv} = {nx(state, x), ny(state, y)}
    state |> draw(@op_point_abs, {nxv, nyv}) |> set_pen({nxv, nyv})
  end

  defp emit_line(state, x, y) do
    {nxv, nyv} = {nx(state, x), ny(state, y)}
    state |> draw(@op_line_abs, {nxv, nyv}) |> set_pen({nxv, nyv})
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

  defp emit_line_set(%{args: args} = c, state, relative) do
    if valid_pairs?(args) do
      {nums, state} = eval_all(args, state)
      pts = norm_points(state, nums)

      if relative do
        # deltas are emitted verbatim; pen is not advanced (mirrors reference)
        draw(state, @op_set_line_rel, pts)
      else
        state |> draw(@op_set_line_abs, pts) |> set_pen(List.last(pts))
      end
    else
      verb = if relative, do: "line-set-rel", else: "line-set"
      add_diag(state, c, "#{verb} needs pairs of coords (got #{length(args)})")
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

  defp emit_domain(%{args: args} = c, state) do
    if length(args) < 2 do
      add_diag(state, c, "'domain' needs at least 2 args")
    else
      {nums, state} = eval_all(args, state)
      [sv, mv | rest] = nums

      dim =
        case rest do
          [d | _] -> trunc(d)
          [] -> 2
        end

      emit_bytes(state, domain_bytes(trunc(sv), trunc(mv), dim))
    end
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

  defp emit_text(%{args: [%{type: :str, value: s} | rest]}, state) do
    # Optional [width height] emits a TEXT attribute command sizing the glyphs.
    state =
      if length(rest) >= 2 do
        {[w, h], state} = eval_all(Enum.take(rest, 2), state)
        %{state | buffer: NaplpsWriter.text_attributes(state.buffer, {w, h})}
      else
        state
      end

    printable = for <<ch <- s>>, ch >= 0x20 and ch <= 0x7E, into: <<>>, do: <<ch>>
    %{state | buffer: NaplpsWriter.draw_text_raw(state.buffer, printable)}
  end

  defp emit_text(c, state),
    do: add_diag(state, c, "text expects a string literal as first arg")

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
    if length(aargs) < 3 do
      add_diag(state, w, "with texture needs (linePattern highlight fillPattern)")
    else
      {[line, hl, fill], state} = eval_all(Enum.take(aargs, 3), state)
      state = emit_bytes(state, texture_bytes(trunc(line), hl != 0, trunc(fill)))
      state = Enum.reduce(body, state, &compile_statement/2)
      # Restore default texture (solid line, no highlight, pattern 0).
      emit_bytes(state, texture_bytes(0, false, 0))
    end
  end

  defp compile_with(%{attr: :domain, attr_args: aargs, body: body} = w, state) do
    if length(aargs) < 2 do
      add_diag(state, w, "with domain needs (singleByte multiByte)")
    else
      {nums, state} = eval_all(aargs, state)
      [sv, mv | rest] = nums

      dim =
        case rest do
          [d | _] -> trunc(d)
          [] -> 2
        end

      state = emit_bytes(state, domain_bytes(trunc(sv), trunc(mv), dim))
      state = Enum.reduce(body, state, &compile_statement/2)
      # Restore defaults (single-byte 1, multi-byte 3, 2D).
      emit_bytes(state, domain_bytes(1, 3, 2))
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
  defp update_pen_from_raw(state, opcode, operands) do
    verts = decode_vertices(operands)
    {px, py} = state.pen

    case opcode &&& 0x7F do
      # Absolute pen sets — pen lands exactly at the (last) decoded vertex.
      op when op in [0x24, 0x26, 0x28] and verts != [] ->
        set_pen(state, List.last(verts))

      # LineSetAbsolute — pen ends at the last absolute vertex.
      0x2A when verts != [] ->
        set_pen(state, List.last(verts))

      # Relative pen offsets / cumulative-relative walks — pen advances by the sum.
      op when op in [0x25, 0x27, 0x29, 0x2B, 0x34, 0x35, 0x3A, 0x3B] ->
        set_pen(state, sum_deltas({px, py}, verts))

      # Arc (mid then end, both relative to pen) — pen ends at the end vertex.
      op when op in [0x2C, 0x2D] and length(verts) >= 2 ->
        [{mdx, mdy}, {edx, edy} | _] = verts
        set_pen(state, {px + mdx + edx, py + mdy + edy})

      # ArcSet (start absolute, mid + end relative) — pen ends at start+dm+de.
      op when op in [0x2E, 0x2F] and length(verts) >= 3 ->
        [{sx, sy}, {dmx, dmy}, {dex, dey} | _] = verts
        set_pen(state, {sx + dmx + dex, sy + dmy + dey})

      # PolygonSet (first vertex absolute, rest relative) — pen ends at the sum.
      op when op in [0x36, 0x37] and length(verts) >= 2 ->
        [start | rels] = verts
        set_pen(state, sum_deltas(start, rels))

      _ ->
        state
    end
  end

  defp sum_deltas(origin, deltas),
    do: Enum.reduce(deltas, origin, fn {dx, dy}, {x, y} -> {x + dx, y + dy} end)

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
