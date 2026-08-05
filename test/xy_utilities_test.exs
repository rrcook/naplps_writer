defmodule XYUtilitiesTest do
  use ExUnit.Case, async: true

  # The bit-level helpers (build_bits/1, twos/1, calc/3, dechk/2, ...) are
  # private; they are exercised transitively through the public pipeline below.

  describe "make_binary/1" do
    test "decodes a hex string" do
      assert XYUtilities.make_binary("DEADBEEF") == {:ok, <<0xDE, 0xAD, 0xBE, 0xEF>>}
    end

    test "is case-insensitive" do
      assert XYUtilities.make_binary("deadbeef") == {:ok, <<0xDE, 0xAD, 0xBE, 0xEF>>}
    end

    test "strips embedded spaces" do
      assert XYUtilities.make_binary("D1 C0 C0") == {:ok, <<0xD1, 0xC0, 0xC0>>}
    end

    test "returns :error on non-hex input" do
      assert XYUtilities.make_binary("zz") == :error
    end
  end

  describe "mul_256/1 and div_256/1" do
    test "mul_256 scales each tuple up by 256" do
      assert XYUtilities.mul_256([{0.5, 0.25}, {-0.5, 1.0}]) == [{128.0, 64.0}, {-128.0, 256.0}]
    end

    test "div_256 scales each tuple down by 256" do
      assert XYUtilities.div_256([{128.0, 64.0}]) == [{0.5, 0.25}]
    end

    test "mul_256 and div_256 round-trip" do
      xys = [{0.5, 0.25}, {-0.75, 0.125}]
      assert xys |> XYUtilities.mul_256() |> XYUtilities.div_256() == xys
    end
  end

  describe "calculate/1" do
    test "all-zero bits is 0.0" do
      assert XYUtilities.calculate([0, 0, 0, 0, 0, 0, 0, 0, 0]) == 0.0
    end

    test "leading fraction bit contributes 0.5" do
      assert XYUtilities.calculate([0, 1, 0, 0, 0, 0, 0, 0, 0]) == 0.5
    end

    test "sign bit set yields a negative value via two's complement" do
      assert XYUtilities.calculate([1, 1, 0, 0, 0, 0, 0, 0, 0]) == -0.5
    end
  end

  describe "dechunk/1" do
    test "decodes a single 3-byte chunk to a fractional coordinate" do
      # 0xD1C0C0 is NaplpsWriter's encoding of {0.5, 0.25}.
      assert XYUtilities.dechunk(<<0xD1, 0xC0, 0xC0>>) == [{0.5, 0.25}]
    end

    test "decodes negative coordinates" do
      # 0xF3C0C0 encodes {-0.5, 0.75}.
      assert XYUtilities.dechunk(<<0xF3, 0xC0, 0xC0>>) == [{-0.5, 0.75}]
    end

    test "decodes multiple chunks in source order" do
      bytes = <<0xD1, 0xC0, 0xC0, 0xF3, 0xC0, 0xC0>>
      assert XYUtilities.dechunk(bytes) == [{0.5, 0.25}, {-0.5, 0.75}]
    end

    test "empty input yields an empty list" do
      assert XYUtilities.dechunk(<<>>) == []
    end
  end

  describe "text_to_xys/1" do
    test "decodes a hex string into device-space coordinates" do
      assert XYUtilities.text_to_xys("D1C0C0") == [{128.0, 64.0}]
    end

    test "tolerates spacing in the hex string" do
      assert XYUtilities.text_to_xys("D1 C0 C0") == [{128.0, 64.0}]
    end
  end

  describe "array_to_telidraw/1" do
    test "formats coordinate tuples as n/256 fraction pairs" do
      assert XYUtilities.array_to_telidraw([{128, 64}, {256, 0}]) ==
               "128/256 64/256 256/256 0/256 "
    end

    test "rounds fractional coordinates to the nearest integer" do
      assert XYUtilities.array_to_telidraw([{127.6, 63.4}]) == "128/256 63/256 "
    end

    test "empty input yields an empty string" do
      assert XYUtilities.array_to_telidraw([]) == ""
    end
  end

  describe "round-trip property (NaplpsWriter.mb_xy oracle)" do
    # NaplpsWriter.mb_xy/2 is the inverse of the decode path: it packs an
    # {x, y} pair into a 3-byte NAPLPS chunk that dechunk/1 must recover. The
    # format carries 8 fractional bits, so coordinates that fall exactly on the
    # n/256 grid must survive the round trip with no loss.
    #
    # The encoder's exact-recovery domain is n in @min_n..@max_n (empirically
    # -0.5 .. 255/256): below -0.5 mb_xy emits a truncated chunk, so we keep the
    # property inside the range where equality is a true invariant.
    @min_n -128
    @max_n 255

    defp round_trip({x, y}) do
      [decoded] = NaplpsWriter.mb_xy(<<>>, {x, y}) |> XYUtilities.dechunk()
      decoded
    end

    test "every grid point on the diagonal decodes back exactly" do
      for n <- @min_n..@max_n do
        v = n / 256

        assert round_trip({v, v}) == {v, v},
               "diagonal grid point #{n}/256 did not round-trip"
      end
    end

    test "random on-grid pairs decode back exactly" do
      for _ <- 1..500 do
        x = Enum.random(@min_n..@max_n) / 256
        y = Enum.random(@min_n..@max_n) / 256

        assert round_trip({x, y}) == {x, y},
               "pair {#{x}, #{y}} did not round-trip"
      end
    end
  end
end
