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
end
