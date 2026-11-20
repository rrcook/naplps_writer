defmodule NaplpsWriterTest do
  use ExUnit.Case
  doctest NaplpsWriter

  describe "def_texture/3 (DEF TEXTURE mask definition)" do
    test "frames 84 <mask> <body> 85 + the code-set restore escape" do
      body = <<0xA4, 0xC0, 0xC0, 0xC9>>
      assert NaplpsWriter.def_texture(<<>>, 0x41, body) ==
               <<0x84, 0x41>> <> body <> <<0x85, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
    end

    test "appends to an existing buffer and accepts every mask A-D" do
      for mask <- 0x41..0x44 do
        assert <<0xBE, 0xD0, 0x84, ^mask, 0x85, 0x1B, 0x29, 0x57, 0x1B, 0x7E>> =
                 NaplpsWriter.def_texture(<<0xBE, 0xD0>>, mask, <<>>)
      end
    end

    test "the body is emitted verbatim (raw, not decoded)" do
      body = NaplpsWriter.draw(<<>>, 0xB1, {2 / 256, 2 / 256})
      assert <<0x84, 0x42, rest::binary>> = NaplpsWriter.def_texture(<<>>, 0x42, body)
      assert rest == body <> <<0x85, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
    end
  end

  describe "def_macro/3 (DEF MACRO definition)" do
    test "frames 80 <code> <body> A5 85" do
      body = <<0xA4, 0xC0, 0xC0, 0xC9>>
      assert NaplpsWriter.def_macro(<<>>, 0x20, body) ==
               <<0x80, 0x20>> <> body <> <<0xA5, 0x85>>
    end

    test "appends to an existing buffer; codes 0x20-0x7F accepted" do
      for code <- [0x20, 0x41, 0x7F] do
        assert <<0xBE, 0xD0, 0x80, ^code, 0xA7, 0xA5, 0x85>> =
                 NaplpsWriter.def_macro(<<0xBE, 0xD0>>, code, <<0xA7>>)
      end
    end

    test "the body is emitted verbatim (re-encoded drawing commands)" do
      body = NaplpsWriter.draw(<<>>, 0xA9, {1 / 256, 1 / 256})
      assert <<0x80, 0x21, rest::binary>> = NaplpsWriter.def_macro(<<>>, 0x21, body)
      assert rest == body <> <<0xA5, 0x85>>
    end
  end

  describe "macro_call/4 (macro invocation)" do
    test "A5 pen-relative position + ESC / z ESC o + code + restore" do
      pos = NaplpsWriter.mb_xy(<<>>, {0.0, 0.0})
      expected =
        <<0xA5>> <> pos <> <<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, 0x20, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
      assert NaplpsWriter.macro_call(<<>>, 0x20, {0.0, 0.0}) == expected
    end

    test "A4 absolute position option" do
      pos = NaplpsWriter.mb_xy(<<>>, {0.5, 0.5})
      expected =
        <<0xA4>> <> pos <> <<0x1B, 0x2F, 0x7A, 0x1B, 0x6F, 0x41, 0x1B, 0x29, 0x57, 0x1B, 0x7E>>
      assert NaplpsWriter.macro_call(<<>>, 0x41, {0.5, 0.5}, pos_op: 0xA4) == expected
    end

    test "ss3 option uses ESC / z SS3" do
      assert <<0xA5, _::binary-size(3), 0x1B, 0x2F, 0x7A, 0x1D, 0x20, 0x1B, 0x29, 0x57, 0x1B, 0x7E>> =
               NaplpsWriter.macro_call(<<>>, 0x20, {0.0, 0.0}, ss3: true)
    end

    test "appends to an existing buffer" do
      assert <<0xBE, 0xD0, 0xA5, _::binary>> =
               NaplpsWriter.macro_call(<<0xBE, 0xD0>>, 0x20, {0.0, 0.0})
    end
  end
end
