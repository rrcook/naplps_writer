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

  describe "serialize_text/2 (GCU text-blob suppression)" do
    test "drops NULs" do
      {bytes, _} = NaplpsWriter.serialize_text(<<"a", 0x00, "b">>)
      assert bytes == "ab"
    end

    test "reverse video 88/89 emitted only on state change" do
      {bytes, _} = NaplpsWriter.serialize_text(<<0x88, "x", 0x88, 0x89, 0x89, 0x88>>)
      assert bytes == <<0x88, "x", 0x89, 0x88>>
    end

    test "underline 99/9A emitted only on state change" do
      {bytes, _} = NaplpsWriter.serialize_text(<<0x99, 0x99, 0x9A, 0x9A>>)
      assert bytes == <<0x99, 0x9A>>
    end

    test "SI only after an ESC (default state: GCU's, esc_seen on at the first text object)" do
      # GCU's r25c2e starts each save with esc_seen set, so a leading SI is kept.
      {bytes, _} = NaplpsWriter.serialize_text(<<0x0F, "a", 0x1B, 0x0F, 0x0F>>)
      assert bytes == <<0x0F, "a", 0x1B, 0x0F>>
    end

    test "SI is dropped when no ESC has been seen" do
      state = %{reverse_video: false, underline: false, esc_seen: false}
      {bytes, _} = NaplpsWriter.serialize_text(<<0x0F, "a", 0x1B, 0x0F, 0x0F>>, state)
      assert bytes == <<"a", 0x1B, 0x0F>>
    end

    test "state persists across calls" do
      {b1, st} = NaplpsWriter.serialize_text(<<0x88>>)
      {b2, _} = NaplpsWriter.serialize_text(<<0x88, 0x89>>, st)
      assert b1 == <<0x88>>
      assert b2 == <<0x89>>
    end

    test "printable text and other controls pass through" do
      {bytes, _} = NaplpsWriter.serialize_text(<<"Hi", 0x0D, 0x0A, 0x86>>)
      assert bytes == <<"Hi", 0x0D, 0x0A, 0x86>>
    end
  end
end
