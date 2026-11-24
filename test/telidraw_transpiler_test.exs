defmodule TelidrawTranspilerTest do
  use ExUnit.Case, async: true

  doctest TelidrawTranspiler

  @examples_dir Path.join(__DIR__, "telidraw_examples")

  # Convenience: compile without the gcu_init header so assertions target only
  # the bytes produced by the source commands.
  defp bare(src), do: TelidrawTranspiler.transpile!(src, init: false)

  describe "example files" do
    for path <- Path.wildcard(Path.join(@examples_dir, "*.td")) do
      name = Path.basename(path)

      test "compiles #{name} without diagnostics" do
        src = File.read!(unquote(path))
        assert {:ok, bytes} = TelidrawTranspiler.transpile(src)
        assert byte_size(bytes) > 0
        # gcu_init header is present by default (starts with DOMAIN opcode 0xA1).
        assert <<0xA1, _::binary>> = bytes
      end
    end
  end

  describe "coordinate / vertex encoding" do
    test "move encodes via NaplpsWriter.mb_xy" do
      bytes = bare("move 0.3 0.4")
      # 0xA4 = PointSetAbsolute, followed by the same 3 bytes mb_xy produces.
      expected_vertex = NaplpsWriter.mb_xy(<<>>, {0.3, 0.4})
      assert bytes == <<0xA4>> <> expected_vertex
    end

    test "goto is an alias for move" do
      assert bare("goto 0.1 0.2") == bare("move 0.1 0.2")
    end

    test "line emits LineAbsolute opcode after a move" do
      # move = 0xA4 + 3 vertex bytes (4 total); line opcode 0xA8 follows.
      assert <<0xA4, _::binary-size(3), 0xA8, _::binary-size(3)>> = bare("move 0 0\nline 0.5 0.5")
    end

    test "line accepts multiple x/y pairs as one polyline" do
      # 3 pairs -> a single 0xA8 opcode followed by 3 encoded vertices (9 bytes).
      pts = [{0.2, 0.2}, {0.3, 0.3}, {0.4, 0.4}]
      expected = <<0xA8>> <> NaplpsWriter.mb_xy(<<>>, pts)
      assert bare("line 0.2 0.2 0.3 0.3 0.4 0.4") == expected
    end

    test "line-rel accepts multiple x/y pairs as one polyline" do
      pts = [{0.1, 0.0}, {0.0, 0.1}, {-0.1, 0.0}]
      expected = <<0xA9>> <> NaplpsWriter.mb_xy(<<>>, pts)
      assert bare("line-rel 0.1 0.0 0.0 0.1 -0.1 0.0") == expected
    end

    test "a single-pair line is unchanged (one vertex)" do
      assert bare("line 0.5 0.5") == <<0xA8>> <> NaplpsWriter.mb_xy(<<>>, {0.5, 0.5})
    end

    test "line requires whole x/y pairs" do
      assert {:error, diags} = TelidrawTranspiler.transpile("line 0.1 0.2 0.3")
      assert Enum.any?(diags, &(&1.message =~ "pairs"))
    end
  end

  describe "color" do
    test "color N selects palette index (0xC0 | index<<2)" do
      # index 4 -> 0xC0 ||| (4<<2) = 0xD0
      assert bare("color 4") == <<0xBE, 0xD0>>
    end

    test "color FG BG emits two operand bytes" do
      # fg=1 -> 0xC4, bg=7 -> 0xDC
      assert bare("color 1 7") == <<0xBE, 0xC4, 0xDC>>
    end

    test "palette alias resolves in color" do
      assert bare("palette cyan = 1\ncolor cyan") == bare("color 1")
    end
  end

  describe "with blocks restore state" do
    test "with color emits set + restore around body" do
      # default color is 7 (0xDC). Inside: color 3 -> 0xC0|(3<<2)=0xCC.
      bytes = bare("with color 3 { move 0 0 }")
      assert <<0xBE, 0xCC, 0xA4, _v::binary-size(3), 0xBE, 0xDC>> = bytes
    end
  end

  describe "attribute commands" do
    test "domain builds the fixed byte (sv, mv, dim)" do
      # domain 1 3 2: data = (0) | ((3-1)<<2) | 0 = 0x08 -> 0xC8
      assert bare("domain 1 3 2") == <<0xA1, 0xC8>>
    end

    test "texture builds the fixed byte" do
      # texture 0 0 0 -> 0xC0 ; highlight false since second arg 0
      assert bare("texture 0 0 0") == <<0xA3, 0xC0>>
      # texture 1 1 2 -> line=1, hl=true(<<2), fill=2(<<3) => 1 | 4 | 16 = 0x15 -> 0xD5
      assert bare("texture 1 1 2") == <<0xA3, 0xD5>>
    end

    test "wait uses GCU's 0xDC fixed byte (1011100 with bit 8 set, as GCU writes it)" do
      # interval byte = 0xC0 | 10 = 0xCA; TITLE.PAG (GCU-written) ends `bd dc ca a5`
      assert bare("wait 10") == <<0xBD, 0xDC, 0xCA>>
    end

    test "reset and nsr" do
      assert bare("reset") == <<0xA0, 0xC0, 0xC0>>
      assert bare("nsr") == <<0x1F>>
    end
  end

  describe "text" do
    test "emits printable ASCII bytes verbatim" do
      assert bare(~s|text "Hi!"|) == "Hi!"
    end

    test "filters non-printable characters" do
      assert bare(~s|text "a\tb"|) == "ab"
    end
  end

  describe "text-raw (control-bearing text)" do
    test "preserves C1 controls authored via \\xHH escapes" do
      assert bare(~S|text-raw "\x88hi\x89"|) == <<0x88, "hi", 0x89>>
    end

    test "preserves CR/LF and REPEAT" do
      assert bare(~S|text-raw "a\x0Db\x0Ac\x86"|) == <<"a", 0x0D, "b", 0x0A, "c", 0x86>>
    end

    test "suppresses redundant toggles (GCU serializer)" do
      assert bare(~S|text-raw "\x88\x88x\x89\x89"|) == <<0x88, "x", 0x89>>
    end

    test "toggle state persists across text-raw commands" do
      src = ~S|text-raw "\x88a"
text-raw "\x88b\x89"|
      assert bare(src) == <<0x88, "a", "b", 0x89>>
    end

    test "moves the pen by GCU's text advance (6/256 per character at GCU's start-up size)" do
      # GCU, measured: text moves the drawing point; 'hi' at the default 6 x 10 field = 12/256.
      src = ~S|move 0.5 0.5
text-raw "\x88hi\x89"
polygon 0.5 0.5|
      back = NaplpsWriter.mb_xy(<<>>, {-12 / 256, 0.0})
      assert <<0xA4, _::binary-size(3), 0x88, "hi", 0x89, 0xB5, rest::binary>> = bare(src)
      assert rest == back
    end

    test "plain text still filters non-printable" do
      assert bare(~s|text "a\tb"|) == "ab"
    end

    test "a leading SI is kept (GCU starts each save as if an ESC had been seen)" do
      assert bare(~S|text-raw "\x0Fab"|) == <<0x0F, "ab">>
    end
  end

  describe "control flow expansion" do
    test "repeat unrolls the body" do
      assert bare("repeat 3 { color 1 }") == bare("color 1\ncolor 1\ncolor 1")
    end

    test "for binds the loop variable and unrolls" do
      # for i in 1..3 { color i } -> color 1, color 2, color 3
      assert bare("for i in 1..3 { color i }") == bare("color 1\ncolor 2\ncolor 3")
    end

    test "for with from > to produces nothing" do
      assert bare("for i in 5..1 { color i }") == <<>>
    end

    test "if selects then/else at compile time" do
      assert bare("if 1 { color 2 } else { color 3 }") == bare("color 2")
      assert bare("if 0 { color 2 } else { color 3 }") == bare("color 3")
    end
  end

  describe "arithmetic and let" do
    test "let binds a variable" do
      assert bare("let x = 0.25\nmove x 0.5") == bare("move 0.25 0.5")
    end

    test "fraction literals and arithmetic evaluate" do
      assert bare("move (1/4) (0.25 + 0.25)") == bare("move 0.25 0.5")
    end

    test "precedence: * before +" do
      assert bare("move (0.1 + 0.2 * 0.5) 0.5") == bare("move 0.2 0.5")
    end
  end

  describe "procs" do
    test "proc call inlines the body with bound params" do
      src = """
      proc dot(px, py) {
        move px py
      }
      dot 0.3 0.4
      """

      assert bare(src) == bare("move 0.3 0.4")
    end

    test "proc params do not leak after the call" do
      src = """
      proc p(a) { color a }
      let a = 5
      p 2
      color a
      """

      # color 2 (from proc), then color 5 (outer a restored)
      assert bare(src) == bare("color 2\ncolor 5")
    end
  end

  describe "polygon and pen tracking" do
    test "polygon converts absolute vertices to pen-relative deltas" do
      # move to (0.5, 0.4); polygon 0.6 0.4 -> relative delta (0.1, 0.0)
      bytes = bare("move 0.5 0.4\npolygon 0.6 0.4")
      move = NaplpsWriter.draw(<<>>, 0xA4, {0.5, 0.4})
      poly = NaplpsWriter.draw(<<>>, 0xB5, [{0.6 - 0.5, 0.4 - 0.4}])
      assert bytes == move <> poly
    end
  end

  describe "raw escape hatch" do
    test "emits operand bytes verbatim" do
      assert bare("raw 164 192 192 192") == <<164, 192, 192, 192>>
    end
  end

  describe "pixel coordinate mode" do
    test "#coord pixels divides by resolution" do
      src = """
      #coord pixels
      #resolution 256 256
      move 64 128
      """

      assert bare(src) == bare("move 0.25 0.5")
    end
  end

  describe "error handling" do
    test "returns diagnostics for unknown identifiers" do
      assert {:error, diags} = TelidrawTranspiler.transpile("move x 0.5")
      assert Enum.any?(diags, &(&1.message =~ "Unknown identifier"))
    end

    test "transpile! raises on error" do
      assert_raise TelidrawTranspiler.Error, fn ->
        TelidrawTranspiler.transpile!("move x 0.5")
      end
    end

    test "reports a parse error for malformed input" do
      assert {:error, _diags} = TelidrawTranspiler.transpile("with { }")
    end
  end

  describe "uppercase mnemonics (raw pass-through)" do
    test "BLUELINE.td (decompiler output with uppercase mnemonics) compiles" do
      path = Path.join([__DIR__, "..", "BLUELINE.td"])
      assert {:ok, bytes} = TelidrawTranspiler.transpile(File.read!(path))
      assert byte_size(bytes) > 0
    end

    test "kebab mnemonic emits opcode + literal operand bytes verbatim" do
      # DOMAIN (0xA1) raw form: args are literal operand bytes, not semantic values.
      assert bare("DOMAIN 200 192 192 201") == <<0xA1, 200, 192, 192, 201>>
    end

    test "lowercase keyword and uppercase mnemonic are different code paths" do
      # lowercase = high-level: domain 1 3 2 -> fixed byte encoding (0xC8)
      assert bare("domain 1 3 2") == <<0xA1, 0xC8>>
      # UPPERCASE = raw: DOMAIN 1 3 2 -> literal operand bytes [1, 3, 2]
      assert bare("DOMAIN 1 3 2") == <<0xA1, 1, 3, 2>>
    end

    test "mnemonic resolution is case-insensitive" do
      # mixed-case misses the lowercase keyword table, resolves as a mnemonic
      assert bare("Domain 1 3 2") == bare("DOMAIN 1 3 2")
      assert bare("line-relative 1 2 3") == bare("LINE-RELATIVE 1 2 3")
    end

    test "ANSI C0 mnemonics resolve (NSR, CAN)" do
      assert bare("NSR 127 79") == <<0x1F, 127, 79>>
      assert bare("CAN") == <<0x18>>
    end

    test "multi-word kebab mnemonic lexes as one identifier" do
      assert bare("POLYGON-SET-FILLED 64 75") == <<0xB7, 64, 75>>
    end

    test "mnemonic with no operands emits just the opcode" do
      assert bare("POINT-SET-RELATIVE") == <<0xA5>>
    end

    test "literal raw form keeps the opcode byte verbatim" do
      # first number IS the opcode; nothing is bit-adjusted.
      assert bare("raw 32 65 66") == <<32, 65, 66>>
    end

    test "a high-level command does not swallow a following mnemonic" do
      # `move` must stop its arg loop at LINE-RELATIVE (the next statement).
      bytes = bare("move 0 0\nLINE-RELATIVE 193 217 196")
      assert <<0xA4, _::binary-size(3), 0xA9, 193, 217, 196>> = bytes
    end

    test "an unknown identifier is still treated as a proc call" do
      # not a mnemonic -> proc call path -> unknown proc diagnostic
      assert {:error, diags} = TelidrawTranspiler.transpile("notacommand 1 2")
      assert Enum.any?(diags, &(&1.message =~ "Unknown procedure"))
    end

    test "raw absolute position command mirrors the pen for later high-level geometry" do
      # POINT-SET-ABSOLUTE with bytes that decode to (0.5, 0.5); a following
      # polygon at (0.5, 0.5) must therefore emit a zero relative delta.
      pt = NaplpsWriter.mb_xy(<<>>, {0.5, 0.5}) |> :binary.bin_to_list()
      src = "POINT-SET-ABSOLUTE #{Enum.join(pt, " ")}\npolygon 0.5 0.5"
      zero = NaplpsWriter.mb_xy(<<>>, {0.0, 0.0})
      assert bare(src) == <<0xA4>> <> :binary.list_to_bin(pt) <> <<0xB5>> <> zero
    end
  end

  describe "def-texture (DEF TEXTURE mask definition)" do
    test "frames 84 <mask> <body> 85 + code-set restore; body geometry via mb_xy" do
      # mask 1 -> 'A' (0x41). Body: move (A4 + vertex) then rect (B1 + vertex).
      body =
        NaplpsWriter.draw(<<>>, 0xA4, {1 / 256, 1 / 256})
        |> NaplpsWriter.draw(0xB1, {2 / 256, 2 / 256})

      expected = <<0x84, 0x41>> <> body <> <<0x85, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
      assert bare("def-texture 1 { move 1/256 1/256 rect 2/256 2/256 }") == expected
    end

    test "masks 1-4 map to the operand bytes A-D" do
      for {n, byte} <- [{1, 0x41}, {2, 0x42}, {3, 0x43}, {4, 0x44}] do
        assert <<0x84, ^byte, 0x85, 0x1B, 0x29, 0x57, 0x1B, 0x7E>> =
                 bare("def-texture #{n} { }")
      end
    end

    test "an empty body still emits the definition framing" do
      assert bare("def-texture 2 { }") == <<0x84, 0x42, 0x85, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
    end

    test "a mask outside 1-4 is diagnosed and emits nothing" do
      assert {:error, diags} = TelidrawTranspiler.transpile("def-texture 5 { }", init: false)
      assert Enum.any?(diags, &(&1.message =~ "mask must be 1-4"))
    end

    test "the definition resets the drawing pen to (0,0), as GCU's decoder does" do
      # move to (0.5,0.5), define a mask, then a polygon at (0.5,0.5). GCU puts the drawing point
      # back at (0,0) when DEF TEXTURE ends (measured under emulation), so the polygon's first
      # relative delta must be (0.5,0.5) for its vertex to land at (0.5,0.5).
      bytes = bare("move 0.5 0.5\ndef-texture 1 { move 0.1 0.1 }\npolygon 0.5 0.5")
      assert <<0xA4, _::binary-size(3), 0x84, 0x41, _::binary-size(4), 0x85,
               0x1B, 0x29, 0x57, 0x1B, 0x7E, 0xB5, rest::binary>> = bytes
      assert rest == NaplpsWriter.mb_xy(<<>>, {0.5, 0.5})
    end
  end

  describe "@macro proc (DEF MACRO definition)" do
    test "emits 80 <code> <body> A5 85; body geometry via mb_xy; auto code 0x20" do
      body = NaplpsWriter.draw(<<>>, 0xA4, {1 / 256, 1 / 256})
      expected = <<0x80, 0x20>> <> body <> <<0xA5, 0x85>>
      assert bare("@macro proc star() { move 1/256 1/256 }") == expected
    end

    test "auto-assigns sequential codes from 0x20" do
      bytes = bare("@macro proc a() { move 0 0 }\n@macro proc b() { move 0 0 }")
      assert <<0x80, 0x20, _::binary-size(4), 0xA5, 0x85,
               0x80, 0x21, _::binary-size(4), 0xA5, 0x85>> = bytes
    end

    test "an explicit code is honored" do
      assert <<0x80, 0x41, _::binary-size(4), 0xA5, 0x85>> =
               bare("@macro proc m() 65 { move 0 0 }")
    end

    test "a body that draws nothing makes no object" do
      assert bare("@macro proc empty() { }") == <<>>
    end

    test "params are bound (to 0) for the definition's concrete geometry" do
      body = NaplpsWriter.draw(<<>>, 0xA4, {0.0, 0.0})
      assert bare("@macro proc m(x) { move x x }") == <<0x80, 0x20>> <> body <> <<0xA5, 0x85>>
    end

    test "an out-of-range code is diagnosed" do
      assert {:error, diags} =
               TelidrawTranspiler.transpile("@macro proc m() 200 { move 0 0 }", init: false)
      assert Enum.any?(diags, &(&1.message =~ "macro code must be"))
    end

    test "a plain proc is inlined, not emitted as a macro" do
      assert bare("proc p() { move 0 0 }\np") == NaplpsWriter.draw(<<>>, 0xA4, {0.0, 0.0})
    end
  end

  describe "@name (macro invocation)" do
    test "define then call: definition blob + A4 position + invoke escape + code + restore" do
      body = NaplpsWriter.draw(<<>>, 0xA4, {1 / 256, 1 / 256})
      defn = <<0x80, 0x20>> <> body <> <<0xA5, 0x85>>
      pos = NaplpsWriter.mb_xy(<<>>, {0.0, 0.0})
      call = <<0xA4>> <> pos <> <<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, 0x20, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
      assert bare("@macro proc m() { move 1/256 1/256 }\n@m") == defn <> call
    end

    test "after a move the call is A5 pen-relative (zero delta at the pen)" do
      bytes = bare("move 0.5 0.5\n@macro proc m() { move 0 0 }\n@m")
      zero = NaplpsWriter.mb_xy(<<>>, {0.0, 0.0})
      assert <<0xA4, _::binary-size(3), 0x80, 0x20, _::binary-size(4), 0xA5, 0x85,
               0xA5, rest::binary>> = bytes
      assert rest == zero <> <<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, 0x20, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
    end

    test "an undefined macro is diagnosed" do
      assert {:error, diags} = TelidrawTranspiler.transpile("@nope", init: false)
      assert Enum.any?(diags, &(&1.message =~ "Unknown macro"))
    end

    test "call args do not change the invocation (GCU plays the recorded body)" do
      assert bare("@macro proc m() { move 0 0 }\n@m 1 2") ==
               bare("@macro proc m() { move 0 0 }\n@m")
    end

    test "two calls resolve the same code; the second is A5" do
      bytes = bare("@macro proc m() { move 0 0 }\n@m\n@m")
      assert <<0x80, 0x20, _::binary-size(4), 0xA5, 0x85,
               0xA4, _::binary-size(3), 0x1B, 0x2F, 0x7A, 0x1B, 0x6F, 0x20, 0x1B, 0x29, 0x57, 0x1B, 0x7E,
               0xA5, _::binary-size(3), 0x1B, 0x2F, 0x7A, 0x1B, 0x6F, 0x20, 0x1B, 0x29, 0x57, 0x1B, 0x7E>> =
               bytes
    end
  end

  # The pen after a macro call is where playing the body leaves it (GCU, measured under
  # emulation: a LINE REL body moves the drawing point by its delta). Each case ends with a
  # polygon whose first vertex is absolute; its encoded delta = vertex - pen, so the tail of the
  # output checks the pen. All values are n/256, exact in the 3-byte encoding.
  describe "pen after a macro call" do
    defp ends_with_polygon_delta(bytes, {dx, dy}) do
      tail = <<0xB5>> <> NaplpsWriter.mb_xy(<<>>, {dx / 256, dy / 256})
      binary_part(bytes, byte_size(bytes) - byte_size(tail), byte_size(tail)) == tail
    end

    test "a LINE REL body moves the pen by its delta" do
      src = "move 64/256 64/256\n@macro proc m() { line-rel 16/256 16/256 }\n@m\npolygon 128/256 128/256"
      # pen after the call = (80,80) -> delta (48,48)
      assert ends_with_polygon_delta(bare(src), {48, 48})
    end

    test "defining a macro does not move the pen (the body is recorded, not run)" do
      src = "move 64/256 64/256\n@macro proc m() { line-rel 16/256 16/256 }\npolygon 128/256 128/256"
      assert ends_with_polygon_delta(bare(src), {64, 64})
    end

    test "a RECT body advances the pen by the width" do
      src = "@macro proc m() { rect 16/256 8/256 }\nmove 64/256 64/256\n@m\npolygon 128/256 128/256"
      # pen after the call = (80,64) -> delta (48,64)
      assert ends_with_polygon_delta(bare(src), {48, 64})
    end

    test "a nested call plays the inner macro twice, as GCU does" do
      # GCU decodes a definition while recording it, so defining b plays a and records a's bytes
      # after the call (GCU's macro store for b: call 20, then a's a9c0d2c0a5, then the rest).
      # Playing b then runs a via the call and again from the copy.
      src = """
      move 0 0
      @macro proc a() { line-rel 16/256 16/256 }
      @macro proc b() {
        @a
        line-rel 16/256 0
      }
      move 64/256 64/256
      @b
      polygon 128/256 128/256
      """
      # (64,64) -> a: (80,80) -> copy of a: (96,96) -> line-rel: (112,96) -> delta (16,32)
      assert ends_with_polygon_delta(bare(src), {16, 32})
    end
  end

  # GCU ends a macro or texture definition at any C1 0x80-0x85, so a definition inside another
  # one would cut the outer short (measured: macro 0x21 with a DEF TEXTURE inside keeps only the
  # bytes before it, and the rest is drawn at top level).
  describe "definitions cannot nest" do
    for {label, src} <- [
          {"def-texture inside a macro", "@macro proc m() {\nline-rel 1/256 1/256\ndef-texture 1 { }\n}"},
          {"a macro inside a macro", "@macro proc m() {\n@macro proc n() { move 0 0 }\n}"},
          {"a macro inside def-texture", "def-texture 1 {\n@macro proc n() { move 0 0 }\n}"},
          {"def-texture inside def-texture", "def-texture 1 {\ndef-texture 2 { }\n}"}
        ] do
      test "#{label} is an error" do
        assert {:error, diags} = TelidrawTranspiler.transpile(unquote(src), init: false)
        assert Enum.any?(diags, &(&1.message =~ "cannot be nested"))
      end
    end
  end

  describe "end of file" do
    test "a complete file ends with GCU's A5" do
      {:ok, bytes} = TelidrawTranspiler.transpile("move 0.5 0.5")
      assert bytes ==
               NaplpsWriter.gcu_init() <> NaplpsWriter.draw(<<>>, 0xA4, {0.5, 0.5}) <> <<0xA5>>
    end

    test "an empty program is the header plus A5" do
      assert TelidrawTranspiler.transpile!("") == NaplpsWriter.gcu_init() <> <<0xA5>>
    end

    test "init: false fragments stay bare (no A5)" do
      assert bare("move 0.5 0.5") == NaplpsWriter.draw(<<>>, 0xA4, {0.5, 0.5})
    end

    test ":terminate overrides the default either way" do
      assert TelidrawTranspiler.transpile!("move 0.5 0.5", terminate: false) ==
               NaplpsWriter.gcu_init() <> NaplpsWriter.draw(<<>>, 0xA4, {0.5, 0.5})

      assert TelidrawTranspiler.transpile!("move 0.5 0.5", init: false, terminate: true) ==
               NaplpsWriter.draw(<<>>, 0xA4, {0.5, 0.5}) <> <<0xA5>>
    end
  end

  defp diag_messages(src) do
    assert {:error, diags} = TelidrawTranspiler.transpile(src, init: false)
    Enum.map(diags, & &1.message)
  end

  # RESET bits as GCU stores and writes them; the pen and compiler state follow GCU's decoder.
  describe "reset flags" do
    test "flags set their bits (first byte: domain, color, screen; second: the rest)" do
      assert bare("reset text macros screen 1") == <<0xA0, 0xC8, 0xD1>>
      assert bare("reset domain color 3 screen 7 text blink fields texture macros drcs") ==
               <<0xA0, 0xFF, 0xFF>>
      assert bare("reset color 1 blink") == <<0xA0, 0xC2, 0xC2>>
    end

    test "only the command's own line is read" do
      assert bare("reset\nmove 0 0") == <<0xA0, 0xC0, 0xC0>> <> bare("move 0 0")
    end

    test "bad flags and values are errors" do
      assert ["reset color must be 0-3, got 4"] = diag_messages("reset color 4")
      assert ["reset screen needs a number"] = diag_messages("reset screen")
      assert [msg] = diag_messages("reset bogus")
      assert msg =~ "got 'bogus'"
    end

    test "reset text sends the pen to the text home (0, 190/256), as GCU does" do
      src = "move 64/256 64/256\nreset text\npolygon 128/256 128/256"
      assert ends_with_polygon_delta(bare(src), {128, -62})
    end

    test "no other bit moves the pen" do
      src = "move 64/256 64/256\nreset domain color 3 screen 7 blink fields texture drcs\n" <>
              "polygon 128/256 128/256"

      assert ends_with_polygon_delta(bare(src), {64, 64})
    end

    test "a RESET text in a macro body moves the pen when the call replays it" do
      src = "@macro proc m() { reset text }\nmove 64/256 64/256\n@m\npolygon 128/256 128/256"
      assert ends_with_polygon_delta(bare(src), {128, -62})
    end

    test "reset macros deletes the macros, so a later call is an error" do
      src = "@macro proc m() { line-rel 1/256 1/256 }\nreset macros\n@m"
      assert ["Unknown macro 'm' (define it with @macro proc)"] = diag_messages(src)
    end

    test "reset macros inside a definition is an error" do
      assert [msg] = diag_messages("@macro proc m() { reset macros }")
      assert msg =~ "restarts the definition"
    end

    test "reset text puts the text style back to the default" do
      bytes = bare("text-style path down\nreset text\ntext \"a\" 1/40 5/128")
      assert String.ends_with?(bytes, NaplpsWriter.text_attributes(<<>>, {1 / 40, 5 / 128}) <> "a")
    end

    test "reset text turns reverse video off, so the next 0x88 is written" do
      assert bare(~s|text-raw "\\x88a"\nreset text\ntext-raw "\\x88b"|) ==
               <<0x88, "a", 0xA0, 0xC0, 0xC1, 0x88, "b">>
    end
  end

  describe "text-style (TEXT fields)" do
    test "named fields set their bits; without size there is no size operand" do
      assert bare("text-style path down cursor block") == <<0xA2, 0xFC, 0xD0>>
    end

    test "size and every field" do
      bytes = bare("text-style size 1/40 5/128 spacing 1 rotation 90 move independent interrow 5/4")
      assert bytes == <<0xA2, 0xC1, 0xCD>> <> NaplpsWriter.mb_xy(<<>>, {1 / 40, 5 / 128})
    end

    test "fields keep their values, and sized text uses them" do
      assert bare("text-style path down\ntext-style cursor block") ==
               <<0xA2, 0xFC, 0xC0, 0xA2, 0xFC, 0xD0>>

      assert bare("text-style spacing 3/2\ntext \"ab\" 1/40 5/128") ==
               <<0xA2, 0xE0, 0xC0, 0xA2, 0xE0, 0xC0>> <>
                 NaplpsWriter.mb_xy(<<>>, {1 / 40, 5 / 128}) <> "ab"
    end

    test "a variable can give a value" do
      assert bare("let r = 90\ntext-style rotation r") == <<0xA2, 0xF1, 0xC0>>
    end

    test "unknown fields and values are errors" do
      assert ["text-style path takes right, left, up, down"] = diag_messages("text-style path up-ish")
      assert ["text-style spacing takes 1, 1.25, 1.5, proportional"] =
               diag_messages("text-style spacing 2")

      assert [msg] = diag_messages("text-style colour 1")
      assert msg =~ "got 'colour'"
    end
  end

  describe "texture mask size and domain pel" do
    test "texture takes an optional mask size" do
      assert bare("texture 1 0 4 1/20 1/20") == <<0xA3, 0xE1>> <> NaplpsWriter.mb_xy(<<>>, {1 / 20, 1 / 20})
      assert ["texture takes line highlight fill [maskWidth maskHeight]"] =
               diag_messages("texture 1 0 4 1/20")

      # GCU writes a negative (mirrored) mask size back positive
      assert [msg] = diag_messages("texture 1 0 4 -1/20 1/20")
      assert msg =~ "negative mask size"
    end

    test "domain takes an optional pel size, at multi-byte 3 and 2-D only" do
      assert bare("domain 1 3 2 2/256 3/256") == <<0xA1, 0xC8>> <> NaplpsWriter.mb_xy(<<>>, {2 / 256, 3 / 256})
      assert [msg] = diag_messages("domain 1 4 2 2/256 2/256")
      assert msg =~ "needs multi-byte 3 and 2-D"
    end

    test "sizes follow the coordinate mode" do
      src = "#coord pixels\n#resolution 256 192\ntexture 1 0 4 16 12"
      assert bare(src) == <<0xA3, 0xE1>> <> NaplpsWriter.mb_xy(<<>>, {16 / 256, 12 / 192})
    end

    test "with blocks put the previous mask size and pel back" do
      assert String.ends_with?(bare("with texture 1 0 4 1/20 1/20 { move 0 0 }"),
               <<0xA3, 0xC0>> <> NaplpsWriter.mb_xy(<<>>, {1 / 16, 1 / 16}))

      assert String.ends_with?(bare("with domain 1 3 2 2/256 2/256 { move 0 0 }"),
               <<0xA1, 0xC8>> <> NaplpsWriter.mb_xy(<<>>, {1 / 256, 1 / 256}))
    end

    test "a with block without a size restores only the fixed byte, as before" do
      assert String.ends_with?(bare("with texture 1 0 4 { move 0 0 }"), <<0xC0, 0xA3, 0xC0>>)
    end
  end

  # GCU's in-text set switches (src/gcu_text.c): SI, ESC n, ESC / } ESC o, ESC / { ESC o.
  describe "text in other character sets" do
    test "mosaic, drcs and supplementary text start with GCU's switch" do
      assert bare(~s|text mosaic "!\\x7f"|) == <<0x1B, 0x2F, 0x7D, 0x1B, 0x6F, 0x21, 0x7F>>
      assert bare(~s|text drcs "AB"|) == <<0x1B, 0x2F, 0x7B, 0x1B, 0x6F, "AB">>
      assert bare(~s|text supplementary "#$"|) == <<0x1B, 0x6E, "#$">>
    end

    test "primary text after another set starts with SI; no switch when the set is in GL" do
      assert bare(~s|text "a"\ntext mosaic "!"\ntext mosaic "?"\ntext "b"|) ==
               <<"a", 0x1B, 0x2F, 0x7D, 0x1B, 0x6F, "!?", 0x0F, "b">>
    end

    test "text primary is plain text" do
      assert bare(~s|text primary "Hi"|) == bare(~s|text "Hi"|)
    end

    test "codes GCU does not keep are errors" do
      assert [msg] = diag_messages(~s|text mosaic "@"|)
      assert msg =~ "0x40-0x5E"
      assert [msg] = diag_messages(~s|text supplementary "A"|)
      assert msg =~ "non-spacing accents"
    end

    test "DEL is a character in the mosaic set only" do
      assert bare(~s|text "a\\x7f"|) == "a"
      assert bare(~s|text mosaic "\\x7f"|) == <<0x1B, 0x2F, 0x7D, 0x1B, 0x6F, 0x7F>>
    end

    test "after text-raw leaves another set in GL, the next text switches back" do
      assert bare(~s|text-raw "\\x1bn#"\ntext "a"|) == <<0x1B, 0x6E, "#", 0x0F, "a">>
    end
  end

  # GCU reads text after an LS3 macro call as macro calls (measured): the call's ESC o leaves the
  # macro set in GL.
  describe "GL after a macro call" do
    @mac "@macro proc m() { line-rel 16/256 16/256 }\nmove 64/256 64/256\n"

    test "text after a call starts with SI" do
      assert String.ends_with?(bare(@mac <> "@m\ntext \"ab\""), <<0x1B, 0x7E, 0x0F, "ab">>)
      assert String.ends_with?(bare(@mac <> "@m\ntext-raw \"ab\""), <<0x1B, 0x7E, 0x0F, "ab">>)
    end

    test "@name ss3 uses the SS3 form, which leaves GL alone" do
      assert String.ends_with?(
               bare(@mac <> "@m ss3\ntext \"ab\""),
               <<0x1B, 0x2F, 0x7A, 0x1D, 0x20, 0x1B, 0x29, 0x57, 0x1B, 0x7E, "ab">>
             )
    end

    test "an SS3 call still re-designates G3, so mosaic text switches again" do
      bytes = bare(@mac <> "text mosaic \"!\"\n@m ss3\ntext mosaic \"?\"")
      assert String.ends_with?(bytes, <<0x1B, 0x7E, 0x1B, 0x2F, 0x7D, 0x1B, 0x6F, "?">>)
    end

    test "an SS3 call moves the pen like an LS3 call" do
      assert ends_with_polygon_delta(bare(@mac <> "@m ss3\npolygon 128/256 128/256"), {48, 48})
    end

    test "text in a macro body starts with SI (the body may play with the macro set in GL)" do
      assert bare("@macro proc m() { text \"x\" }\ntext \"y\"") ==
               <<0x80, 0x20, 0x0F, "x", 0xA5, 0x85, "y">>
    end
  end

  # Where GCU's drawing point is after each command (measured under emulation: a polygon with a
  # zero first delta starts at the pen). Each test ends with `polygon 128/256 128/256`, whose delta
  # is (128,128) minus the compiler's pen, in 1/256 units.
  describe "pen after each command (GCU)" do
    @p "\npolygon 128/256 128/256"

    test "move-rel and point-rel move the pen by the delta" do
      assert ends_with_polygon_delta(bare("move 64/256 64/256\nmove-rel 16/256 8/256" <> @p), {48, 56})
      assert ends_with_polygon_delta(bare("move 64/256 64/256\npoint-rel 16/256 8/256" <> @p), {48, 56})
    end

    test "line-rel moves the pen by all its deltas" do
      assert ends_with_polygon_delta(bare("move 64/256 64/256\nline-rel 16/256 8/256 8/256 0" <> @p), {40, 56})
    end

    test "rect-outline and rect-set advance by the width" do
      assert ends_with_polygon_delta(bare("move 64/256 64/256\nrect-outline 16/256 8/256" <> @p), {48, 64})
      assert ends_with_polygon_delta(bare("rect-set 32/256 32/256 16/256 8/256" <> @p), {80, 96})
    end

    test "after a polygon the pen is back at its start" do
      src = "move 64/256 64/256\npolygon 96/256 64/256 80/256 96/256" <> @p
      assert ends_with_polygon_delta(bare(src), {64, 64})
      assert ends_with_polygon_delta(bare("polygon-set 32/256 32/256 64/256 32/256 48/256 64/256" <> @p), {96, 96})
    end

    test "a FIELD puts the pen at its origin; a full-screen FIELD at (0,0)" do
      assert ends_with_polygon_delta(bare("move 64/256 64/256\nfield 32/256 40/256 128/256 64/256" <> @p), {96, 88})
      assert ends_with_polygon_delta(bare("move 64/256 64/256\nfield" <> @p), {128, 128})
    end

    test "text moves the pen: 6/256 per character at GCU's start-up size" do
      assert ends_with_polygon_delta(bare("move 64/256 64/256\ntext \"Hello\"" <> @p), {34, 64})
    end

    test "proportional text uses GCU's widths" do
      # GCU, measured: 'Hello world' is 90/256 at a 12-wide field (char_advance/2 gives 86.2 there)
      src = "move 20/256 64/256\ntext \"Hello world\" 12/256 16/256" <> @p
      assert ends_with_polygon_delta(bare(src), {18, 64})
    end

    test "text that continues keeps GCU's fine cursor; the pen is its whole part" do
      # 5/4 spacing at a 6-wide field: 7.5/256 per character; 64 + 3 * 7.5 = 86.5 -> pen 86
      src = "text-style spacing 5/4\nmove 64/256 64/256\ntext \"Hi\" 6/256 10/256\ntext \"i\"" <> @p
      assert ends_with_polygon_delta(bare(src), {42, 64})
    end

    test "text starts where the decoder's pen is, the rounded encoded position" do
      # move 0.3 is encoded as 77/256; 'Hello' -> 107/256
      assert ends_with_polygon_delta(bare("move 0.3 0.3\ntext \"Hello\"" <> @p), {21, 51})
    end
  end

  # GCU reads further SET&LINE operands as new (set, line) pairs, so a longer run is split.
  describe "multi-segment line-set" do
    test "line-set is SET&LINE to the first vertex, then LINE ABS for the rest" do
      bytes = bare("line-set 32/256 32/256 48/256 40/256 56/256 48/256")
      v = fn x, y -> NaplpsWriter.mb_xy(<<>>, {x / 256, y / 256}) end
      assert bytes == <<0xAA>> <> v.(32, 32) <> v.(48, 40) <> <<0xA8>> <> v.(56, 48)
    end

    test "line-set-rel is SET&LINE REL with the first delta, then LINE REL for the rest" do
      bytes = bare("line-set-rel 32/256 32/256 16/256 8/256 8/256 8/256\npolygon 128/256 128/256")
      v = fn x, y -> NaplpsWriter.mb_xy(<<>>, {x / 256, y / 256}) end
      assert String.starts_with?(bytes, <<0xAB>> <> v.(32, 32) <> v.(16, 8) <> <<0xA9>> <> v.(8, 8))
      # the pen ends at the start plus every delta: (56, 48)
      assert ends_with_polygon_delta(bytes, {72, 80})
    end
  end

  # GCU's reader (src/naplps_read.c): a LINE REL object starts at its line pen, which can differ
  # from the drawing point; text after a LINE REL run loses the run; text resumed on the row of the
  # previous text object is padded; SELECT COLOR with equal colours keeps the old foreground. The
  # compiler adds the bytes that make GCU read each case as written (measured under emulation).
  describe "GCU's reader" do
    @sync <<0xA5, 0xC0, 0xC0, 0xC0>>
    @line_rel <<0xA9, 0xC0, 0xC8, 0xD0>>

    test "a LINE REL first in a complete file is preceded by POINT SET REL (0,0)" do
      # after gcu_init GCU's line pen is the screen centre (128,100)/256
      assert TelidrawTranspiler.transpile!("line-rel 10/256 0") ==
               NaplpsWriter.gcu_init() <> @sync <> @line_rel <> <<0xA5>>

      # a fragment assumes the caller's context has it at the drawing point
      assert bare("line-rel 10/256 0") == @line_rel
    end

    test "a LINE REL after a polygon, text or DEF TEXTURE is synced" do
      for pre <- [
            "polygon 100/256 100/256 140/256 100/256 140/256 140/256",
            "move 60/256 60/256\ntext \"abc\"",
            "def-texture 1 {\n  rect 2/256 2/256\n}"
          ] do
        assert String.ends_with?(bare(pre <> "\nline-rel 10/256 0"), @sync <> @line_rel), pre
      end
    end

    test "a LINE REL after moves, lines, arcs, rectangles and calls needs no sync" do
      for pre <- [
            "move 60/256 60/256",
            "line 40/256 40/256",
            "arc 100/256 100/256 140/256 100/256",
            "rect 8/256 8/256",
            "@macro proc m() { polygon 0 0 16/256 0 16/256 16/256 }\nmove 64/256 64/256\n@m"
          ] do
        bytes = bare(pre <> "\nline-rel 10/256 0")
        assert String.ends_with?(bytes, @line_rel), pre
        refute String.ends_with?(bytes, @sync <> @line_rel), pre
      end
    end

    test "a macro body's first LINE REL is synced before the definition, later ones in the body" do
      bytes = bare("polygon 100/256 100/256 140/256 100/256\n@macro proc m() { line-rel 4/256 4/256 }")
      assert String.ends_with?(bytes, @sync <> <<0x80, 0x20, 0xA9>> <> NaplpsWriter.mb_xy(<<>>, {4 / 256, 4 / 256}) <> <<0xA5, 0x85>>)

      # a body that does not start with a LINE REL needs nothing before the definition
      assert bare("polygon 100/256 100/256 140/256 100/256\n@macro proc m() { move 0 0 }") ==
               bare("polygon 100/256 100/256 140/256 100/256") <> bare("@macro proc m() { move 0 0 }")

      body = bare("@macro proc m() {\n  polygon 0 0 16/256 0 16/256 16/256\n  line-rel 4/256 4/256\n}")
      assert :binary.match(body, @sync <> <<0xA9>>) != :nomatch
    end

    test "text right after a LINE REL run closes the run first" do
      bytes = bare("move 108/256 88/256\nline-rel 0 20/256\ntext \"cw\"")
      assert String.ends_with?(bytes, @sync <> "cw")

      # a colour between does not close it either
      assert String.ends_with?(bare("move 108/256 88/256\nline-rel 0 20/256\ncolor 4\ntext \"cw\""), @sync <> "cw")
    end

    test "text resumed on the row of the previous text object starts a new field" do
      at = fn x, y -> <<0xB8, 0xA4>> <> NaplpsWriter.mb_xy(<<>>, {x / 256, y / 256}) end
      text = "move 108/256 88/256\ntext \"abc\"\n"

      # after TEXTURE (the cursor is at 108 + 3 * 6) and after a move to the right
      assert String.ends_with?(bare(text <> "texture 3 0 7\ntext \"cw\""), at.(126, 88) <> "cw")
      assert String.ends_with?(bare(text <> "move 150/256 88/256\ntext \"cw\""), at.(150, 88) <> "cw")

      # not to the left, not on another row, not when the text continues (a colour keeps it open)
      for rest <- ["move 60/256 88/256\ntext \"cw\"", "move 108/256 100/256\ntext \"cw\"", "color 3\ntext \"cw\""] do
        assert :binary.match(bare(text <> rest), <<0xB8>>) == :nomatch, rest
      end

      # nor inside a custom field (GCU pads there from the field's edge by design)
      src = "field 20/256 20/256 100/256 60/256\ntext \"ab\"\nmove 60/256 20/256\ntext \"cd\""
      assert String.ends_with?(bare(src), <<0xA4>> <> NaplpsWriter.mb_xy(<<>>, {60 / 256, 20 / 256}) <> "cd")
    end

    test "equal foreground and background colours are preceded by the foreground alone" do
      assert bare("color 9 9") == <<0xBE, 0xE4, 0xBE, 0xE4, 0xE4>>
      assert bare("color 9 3") == <<0xBE, 0xE4, 0xCC>>
    end

    test "text-raw checks the codes GCU does not keep in the set GL holds" do
      assert diag_messages("text mosaic \"\\x21\"\ntext-raw \"N\"") |> Enum.any?(&(&1 =~ "mosaic codes"))
      assert diag_messages("text supplementary \"\\x21\"\ntext-raw \"E\"") |> Enum.any?(&(&1 =~ "accents"))
      assert TelidrawTranspiler.transpile("text-raw \"NE\"", init: false) == {:ok, "NE"}
    end
  end

  # gcu_form: true writes GCU's normalized form (NaplpsGcuForm); the expected bytes are GCU's own
  # re-save of the plain output under emulation (scratch p_palette_probe / p_gcuform_xcheck).
  describe "gcu_form" do
    test "attributes on change, SET& forms, the end A5 (GCU's default: palette changes dropped)" do
      src = "color 3\nset-color 200 100 50\nline 40/256 40/256"

      assert TelidrawTranspiler.transpile!(src, gcu_form: true) ==
               Base.decode16!("A1C8C0C0C9A3C0C0D2C0BECCABC0C0C0C0EDC0A5")
    end

    test "palette: true writes the changed palette entries before each object (GCU's DS:8418)" do
      src = "color 3\nset-color 200 100 50\nline 40/256 40/256"

      assert TelidrawTranspiler.transpile!(src, gcu_form: true, palette: true) ==
               Base.decode16!("BECCBCE2C0C0A1C8C0C0C9A3C0C0D2C0BECCABC0C0C0C0EDC0A5")

      # RESET's palette reset restores entry 3, and keeps its palette bits
      src2 = src <> "\nreset color 1\nline 80/256 40/256"

      assert TelidrawTranspiler.transpile!(src2, gcu_form: true, palette: true) ==
               Base.decode16!(
                 "BECCBCE2C0C0A1C8C0C0C9A3C0C0D2C0BECCABC0C0C0C0EDC0BECCBCC7F8FFBEDCA0C2C0ABC0EDC0C0E8C0A5"
               )

      assert TelidrawTranspiler.transpile!(src2, gcu_form: true) ==
               Base.decode16!("A1C8C0C0C9A3C0C0D2C0BECCABC0C0C0C0EDC0BEDCA0C0C0ABC0EDC0C0E8C0A5")
    end

    test "what GCU would change instead of keep is a diagnostic" do
      assert {:error, [%{message: msg}]} =
               TelidrawTranspiler.transpile("text-style rotation 90\nmove 64/256 64/256\ntext \"a\"", gcu_form: true)

      assert msg =~ "gcu_form: rotated text"

      assert {:error, [%{message: "gcu_form needs a complete file (init: true)"}]} =
               TelidrawTranspiler.transpile("line 0 0", gcu_form: true, init: false)
    end
  end
end
