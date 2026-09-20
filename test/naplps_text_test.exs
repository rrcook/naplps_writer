defmodule NaplpsTextTest do
  use ExUnit.Case
  doctest NaplpsText

  describe "metrics" do
    test "advance varies by width class" do
      assert NaplpsText.char_advance(6, ?W) > NaplpsText.char_advance(6, ?i)
      assert NaplpsText.char_advance(6, ?m) > NaplpsText.char_advance(6, ?l)
    end

    test "at char width 6 the advance is the raw displacement" do
      # n == 6, so the charW/n factor cancels.
      assert NaplpsText.char_advance(6, ?i) == 2.0
    end

    test "a width below 6 still measures, clamped to the first row" do
      # The body font is 5 wide; n clamps to 6 and scales by 5/6.
      assert_in_delta NaplpsText.char_advance(5, ?i), 5 * 2 / 6, 1.0e-9
    end

    test "text_width sums its characters and is empty for an empty string" do
      assert NaplpsText.text_width(6, "") == 0.0

      assert_in_delta NaplpsText.text_width(6, "ab"),
                      NaplpsText.char_advance(6, ?a) + NaplpsText.char_advance(6, ?b),
                      1.0e-9
    end

    test "proportional, not monospaced - equal counts differ in width" do
      assert NaplpsText.text_width(5, "Illinois") != NaplpsText.text_width(5, "iiiiiiii")
    end

    test "unprintable characters fall back to the widest class" do
      assert NaplpsText.width_class(0x07) == 9
    end
  end

  describe "hyphenation_points/1" do
    test "splits a doubled consonant between the pair" do
      assert NaplpsText.hyphenation_points("running") == [3]
      assert NaplpsText.hyphenation_points("stopping") == [4]
    end

    test "never leaves a doubled consonant ending the fragment" do
      # "runn-ing" is available from the -ing suffix rule and must lose to
      # "run-ning".
      refute 4 in NaplpsText.hyphenation_points("running")
    end

    test "keeps digraphs together" do
      # "Was-hington" splits the sh; the break belongs after it.
      assert NaplpsText.hyphenation_points("Washington") == [4]
    end

    test "finds prefix and suffix boundaries" do
      assert 5 in NaplpsText.hyphenation_points("international")
      assert 7 in NaplpsText.hyphenation_points("development")
    end

    test "does not offer -ent over the correct -ment break" do
      refute 8 in NaplpsText.hyphenation_points("development")
    end

    test "declines short words, acronyms, digits and existing hyphens" do
      assert NaplpsText.hyphenation_points("cat") == []
      assert NaplpsText.hyphenation_points("NATO") == []
      assert NaplpsText.hyphenation_points("x1234y") == []
      assert NaplpsText.hyphenation_points("US-Jordan") == []
    end

    test "respects the prefix and suffix minimums" do
      for word <- ~w(running carpet development international summarization),
          at <- NaplpsText.hyphenation_points(word) do
        assert at >= 2, "#{word} broke with fewer than 2 leading characters"
        assert String.length(word) - at >= 3, "#{word} stranded fewer than 3"
      end
    end
  end

  describe "wrap/4" do
    @body "New Mexico Democrats are rejecting President Trump posts suggesting the state be renamed New America. Governor Michelle Lujan Grisham said the name is not up for debate."

    test "short text stays on one line" do
      assert NaplpsText.wrap("hello world", 6, 1000) == ["hello world"]
    end

    test "every line fits the field" do
      for line <- NaplpsText.wrap(@body, 5, 250) do
        assert NaplpsText.text_width(5, line) <= 250
      end
    end

    test "no words are lost" do
      words = fn s -> s |> String.replace("-", "") |> String.split(~r/\s+/, trim: true) end

      assert @body
             |> NaplpsText.wrap(5, 250, hyphenate: false)
             |> Enum.join(" ")
             |> words.() == words.(@body)
    end

    test "hyphenation yields fewer or equal lines than not hyphenating" do
      with_h = length(NaplpsText.wrap(@body, 5, 250, break_capitalized: true))
      without = length(NaplpsText.wrap(@body, 5, 250, hyphenate: false))
      assert with_h <= without
    end

    test "capitalized words are kept whole by default" do
      assert Enum.all?(NaplpsText.wrap(@body, 5, 250), &(not String.contains?(&1, "Miche-")))
    end

    test "a word longer than the line is hard-split rather than overflowing" do
      lines = NaplpsText.wrap("antidisestablishmentarianism", 5, 60)
      assert length(lines) > 1
      for line <- lines, do: assert(NaplpsText.text_width(5, line) <= 60)
    end

    test "a hyphenated word breaks at its own hyphen, adding nothing" do
      max = NaplpsText.text_width(6, "a long-") + 0.5
      assert NaplpsText.wrap("a long-term plan", 6, max) == ["a long-", "term", "plan"]
    end

    test "a hyphenated word too wide for any line still breaks at its own hyphen" do
      # Hyphenation off, so this reaches hard_split/4 - which must not add a
      # second hyphen ("long-t-") when the word has one of its own.
      max = NaplpsText.text_width(6, "long-") + 0.5
      assert NaplpsText.wrap("long-term", 6, max, hyphenate: false) == ["long-", "term"]
    end

    test "a word longer than two lines is wrapped to the end, with no line over-wide" do
      max = NaplpsText.text_width(6, "abcdefgh")

      for text <- ["Supercalifragilisticexpialidocious", "antidisestablishmentarianism is long"] do
        lines = NaplpsText.wrap(text, 6, max)
        assert length(lines) > 2

        for line <- lines, do: assert(NaplpsText.text_width(6, line) <= max)

        # Nothing lost: every letter survives, only hyphens were added.
        letters = &String.replace(&1, ~r/[\s-]/, "")
        assert letters.(Enum.join(lines, " ")) == letters.(text)
      end
    end

    test "collapses runs of whitespace" do
      assert NaplpsText.wrap("a  \n  b", 6, 1000) == ["a b"]
    end
  end
end
