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

    test "wait uses the mandated 0x5C fixed byte" do
      # interval byte = 0xC0 | 10 = 0xCA
      assert bare("wait 10") == <<0xBD, 0x5C, 0xCA>>
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
end
