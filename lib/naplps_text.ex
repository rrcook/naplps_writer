defmodule NaplpsText do
  # Copyright 2026, Ralph Richard Cook & Phillip Heller
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

  @moduledoc """
  Text measurement and line breaking for the NAPLPS proportional font.

  The NAPLPS text font is proportionally spaced, so a character count says
  little about how wide a string actually draws. Anything that has to fit text
  into a known area - centering a headline, right-aligning a byline, deciding
  where a line of body copy ends - needs real advance widths.

  `text_width/2` measures; `wrap/4` breaks a paragraph into lines that fit.

  ## Where the numbers come from

  The advance of a character is a function of its *width class* and the text
  size:

      advance = char_width * disp[clamp(char_width, 6, 11) - 6][width_class(c)]
                           / clamp(char_width, 6, 11)

  The two tables are transcribed from the FoxCouncil NAPLPS renderer
  (`NAPLPS/Drawing/DrawableAsciiChar.cs`), which is a deliberate match to the
  output of period renderers. Its own notes say the strict reading of
  ANSI X3.110 produces visibly different spacing, so treat these as "what the
  real terminals did" rather than "what the standard says".

  Widths come out in the same units as `char_width`, which for Prodigy work is
  GCU units - the numerators of the `n/256` coordinates used elsewhere in this
  library.

  ## Which character size

  The tables are not measured from any one font. They give spacing keyed on
  the width of the character cell, `char_width`, with a row for each cell
  width from 6 to 11 units. The caller picks the text size by passing its cell
  width, so one set of functions serves every size.

  A cell outside 6..11 uses the nearest row and scales the result linearly.
  That makes a 6x10 cell exact (row 6 as-is) and a 5x9 cell an approximation
  (row 6 scaled by 5/6). The renderers switch to a different spacing rule at 12
  and above; this module does not model it, so treat sizes of 12 or more as
  approximate too.

  ## How the pieces fit

    * `char_advance/2` - width of one character: look up its width class, then
      the displacement for that class at this cell width.
    * `text_width/2` - width of a string: the sum of its characters' advances.
    * `hyphenation_points/1` - where a word may be split with a hyphen, from
      simple spelling rules (no dictionary).
    * `wrap/4` - splits a paragraph into lines no wider than a limit, using the
      two above: measure as it fills each line, and break a word that would
      overflow - at its own hyphen or a hyphenation point - if part of it
      still fits.
  """

  # Width class per ASCII character, 0x20..0x7E: one entry per character, in
  # code order, 0 = narrowest ("i", "l", "!") through 9 = widest ("W", "M").
  # The class is not a width by itself; @displacement turns it into one for a
  # given cell width. Indexed by `char - 0x20` in width_class/1.
  @ascii_width_class {
    # 0x20-0x2F   space ! " # $ % & ' ( ) * + , - . /
    9,
    0,
    4,
    6,
    9,
    9,
    9,
    0,
    1,
    1,
    9,
    9,
    3,
    5,
    0,
    9,
    # 0x30-0x3F   0 1 2 3 4 5 6 7 8 9 : ; < = > ?
    5,
    1,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    0,
    3,
    5,
    8,
    5,
    8,
    # 0x40-0x4F   @ A B C D E F G H I J K L M N O
    9,
    5,
    5,
    5,
    5,
    5,
    5,
    8,
    5,
    2,
    5,
    5,
    5,
    9,
    5,
    9,
    # 0x50-0x5F   P Q R S T U V W X Y Z [ \\ ] ^ _
    5,
    6,
    5,
    5,
    9,
    5,
    9,
    9,
    9,
    9,
    9,
    4,
    9,
    4,
    2,
    9,
    # 0x60-0x6F   ` a b c d e f g h i j k l m n o
    1,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    0,
    4,
    5,
    0,
    9,
    5,
    5,
    # 0x70-0x7E   p q r s t u v w x y z { | } ~
    5,
    5,
    5,
    5,
    2,
    5,
    9,
    9,
    9,
    5,
    5,
    5,
    0,
    5,
    9
  }

  # How far the pen moves after a character, by cell width and width class.
  # Row 0 is a cell 6 units wide, row 5 a cell 11 wide; column N is width
  # class N. Values are in the same units as the cell width, so at row 0 a
  # class-9 character advances 6 (the full cell) and a class-0 character 2.
  @displacement {
    {2, 3, 4, 3, 4, 5, 6, 4, 5, 6},
    {3, 4, 5, 4, 5, 6, 7, 5, 6, 7},
    {2, 3, 4, 4, 5, 6, 7, 6, 7, 8},
    {3, 4, 5, 5, 6, 7, 8, 7, 8, 9},
    {4, 5, 6, 6, 7, 8, 9, 8, 9, 10},
    {3, 4, 6, 6, 7, 8, 10, 8, 10, 11}
  }

  # Anything outside printable ASCII falls back to the widest class.
  @fallback_class 9

  @doc """
  Advance width of one character at the given character-field width.

      iex> NaplpsText.char_advance(6, ?W) > NaplpsText.char_advance(6, ?i)
      true
  """
  @spec char_advance(number(), char()) :: float()
  def char_advance(char_width, char) when is_number(char_width) and is_integer(char) do
    # Choose the table row for this cell width. The table only has rows for
    # widths 6..11, so anything outside that range borrows the nearest row.
    n = char_width |> trunc() |> max(6) |> min(11)

    # Row by cell width, column by the character's width class.
    disp = @displacement |> elem(n - 6) |> elem(width_class(char))

    # `disp` is measured against a cell n wide. Scale it to the real cell
    # width: a no-op inside 6..11, and what keeps a 5-wide cell (which
    # borrowed row 6) in proportion.
    char_width * disp / n
  end

  @doc """
  Width of a string, in the same units as `char_width`.

      iex> NaplpsText.text_width(6, "") == 0
      true
  """
  @spec text_width(number(), String.t()) :: float()
  def text_width(char_width, text) when is_binary(text) do
    # The pen advances by each character in turn, so a string is exactly as
    # wide as the sum of its characters' advances.
    text
    |> to_charlist()
    |> Enum.reduce(0.0, fn c, acc -> acc + char_advance(char_width, c) end)
  end

  @doc "Width class (0-9) of a character; the widest class for anything unprintable."
  @spec width_class(char()) :: 0..9
  # The class table starts at space (0x20), hence the offset.
  def width_class(char) when char >= 0x20 and char <= 0x7E,
    do: elem(@ascii_width_class, char - 0x20)

  def width_class(_char), do: @fallback_class

  # --- Hyphenation ----------------------------------------------------------

  # Typographic minimums: never strand fewer than this many letters on either
  # side of the break. Two before and three after is the usual English setting.
  @min_prefix 2
  @min_suffix 3
  # Shorter words are never hyphenated at all: there is too little to move.
  @min_word 6

  @vowels ~c"aeiouy"

  # Consonant pairs that spell one sound. A break never falls between them; it
  # goes after the pair instead, so "Washington" gives "Wash-ington" rather
  # than "Was-hington".
  @digraphs ~w(sh ch th ph wh gh ck ng qu)

  # Prefixes that take a break immediately after them. Every prefix a word
  # starts with becomes a candidate (see prefix_points/1), so "understand"
  # offers both "un-" and "under-"; the order here does not matter.
  @prefixes ~w(inter under over trans super semi anti auto multi
               dis pre non mis sub out per pro con com
               un re in im ex de en em)

  # Suffixes that take a break immediately before them, longest first.
  # Only suffixes of 3+ characters survive the @min_suffix filter, so the short
  # ones here are documentation of intent rather than live rules. "ent"/"ant"
  # are deliberately absent: they fire inside "development" and "important",
  # where -ment and -ance already give the correct break.
  @suffixes ~w(ationally ability tional ations ction ssion ution ition ation
               ment ness able ible less ical ance ence ings tion sion
               ing ers est ful ily ies ous ive ial ual
               ly ed er es al ic)

  @doc """
  Candidate hyphenation offsets for a word, as character positions where a
  hyphen may be inserted.

  Rule-based rather than dictionary-based: this is a small library and a
  pattern dictionary would dwarf it. The rules are conservative and prefer
  making no suggestion over making a wrong one - a missed opportunity only
  costs raggedness, while a bad break is visible in the copy.

  Never suggests anything for a word that is short, contains a digit, is all
  capitals (an acronym), or already contains a hyphen - `wrap/4` breaks a
  hyphenated word at the hyphen it already has instead of adding another.

      iex> NaplpsText.hyphenation_points("running") != []
      true
      iex> NaplpsText.hyphenation_points("cat")
      []
  """
  @spec hyphenation_points(String.t()) :: [pos_integer()]
  def hyphenation_points(word) when is_binary(word) do
    chars = to_charlist(word)
    len = length(chars)

    cond do
      # Too short to be worth splitting.
      len < @min_word -> []
      # Numbers, dates, model numbers: splitting these misleads the reader.
      Enum.any?(chars, &(&1 in ?0..?9)) -> []
      # Already hyphenated: do not add a second hyphen. wrap/4 breaks at the
      # existing one instead (existing_hyphen_points/1).
      String.contains?(word, "-") -> []
      # All capitals: an acronym, which reads worse split than moved.
      word == String.upcase(word) -> []
      # Otherwise collect every rule's suggestions. Rules often agree on the
      # same position, so drop duplicates, and return them left to right.
      true -> chars |> candidates(len) |> Enum.uniq() |> Enum.sort()
    end
  end

  # Every position a hyphen may go in a word, from all three rule families,
  # minus the ones the length limits or the doubled-consonant rule forbid.
  #
  # `chars` is the word as a charlist and `len` its length. A position is a
  # count of leading letters: 3 means the hyphen goes after the third letter.
  defp candidates(chars, len) do
    # The rules are written in lowercase; compare in lowercase so a
    # capitalized word matches them too.
    lower = Enum.map(chars, &lower/1)

    # A tuple gives constant-time access by position for ends_doubled?/2.
    arr = List.to_tuple(lower)

    (prefix_points(lower) ++ suffix_points(lower, len) ++ cluster_points(lower, len))
    # Leave at least @min_prefix letters before the break and @min_suffix after.
    |> Enum.filter(&(&1 >= @min_prefix and len - &1 >= @min_suffix))
    # Refuse a break that would leave a doubled consonant hanging ("runn-ing").
    |> Enum.reject(&ends_doubled?(arr, &1))
  end

  # One candidate per known prefix the word starts with, placed right after
  # the prefix: "understand" gives 2 ("un") and 5 ("under").
  #
  # The match is on spelling alone, so a word that merely begins with these
  # letters gets a candidate too, and some of those are wrong ("un-ique",
  # "re-ason"). Most prefix candidates are right, though, and a vowel-based
  # filter was tried and rejected: it removed about as many correct breaks
  # ("re-order", "ex-ample") as wrong ones.
  defp prefix_points(lower) do
    word = List.to_string(lower)

    @prefixes
    |> Enum.filter(&String.starts_with?(word, &1))
    |> Enum.map(&String.length/1)
  end

  # One candidate per known suffix the word ends with, placed right before the
  # suffix: "development" gives 7 ("ment"). Measured from the end, hence `len`.
  defp suffix_points(lower, len) do
    word = List.to_string(lower)

    @suffixes
    |> Enum.filter(&String.ends_with?(word, &1))
    |> Enum.map(&(len - String.length(&1)))
  end

  # Candidates from the shape of the word rather than known prefixes or
  # suffixes. Two rules carry most of English: split a doubled consonant
  # (run-ning), and split between the two consonants of a
  # vowel-consonant-consonant-vowel run (car-pet). Also "-le" endings, which
  # take the consonant before the "l" with them (untan-gle). A five-letter word
  # like "table" is under @min_word, so it is never split at all.
  #
  # Returns positions in the same sense as candidates/2.
  defp cluster_points(lower, len) do
    idx = Enum.with_index(lower)
    arr = List.to_tuple(lower)

    # Position i where letter i repeats letter i-1: the break goes between the
    # pair. First and last letters are excluded.
    doubled =
      for {c, i} <- idx,
          i > 0,
          i < len - 1,
          consonant?(c),
          elem(arr, i - 1) == c,
          do: i

    # Position i where letters i-1..i+2 are vowel, consonant, consonant,
    # vowel. Normally the break goes between the consonants (i+1). If they
    # are a digraph such as "sh", it goes after both instead (i+2), so the
    # pair stays together.
    vccv =
      for i <- 1..max(len - 3, 1),
          i + 2 < len,
          vowel?(elem(arr, i - 1)),
          consonant?(elem(arr, i)),
          consonant?(elem(arr, i + 1)),
          vowel?(elem(arr, i + 2)) do
        if digraph?(elem(arr, i), elem(arr, i + 1)), do: i + 2, else: i + 1
      end

    # A word ending consonant + "le" breaks before that consonant.
    consonant_le =
      if len >= 4 and Enum.take(lower, -2) == ~c"le" and consonant?(elem(arr, len - 3)),
        do: [len - 3],
        else: []

    doubled ++ vccv ++ consonant_le
  end

  # Lowercase one ASCII letter; anything else passes through unchanged.
  defp lower(c) when c >= ?A and c <= ?Z, do: c + 32
  defp lower(c), do: c

  # A break must not leave a doubled consonant at the end of the fragment: the
  # correct break for "running" is between the n's, and that candidate is
  # already on the list, so "runn-ing" is only ever the worse of the two.
  #
  # True when the two letters just before position `at` are the same
  # consonant. `arr` is the lowercased word as a tuple.
  defp ends_doubled?(arr, at) when at >= 2 do
    a = elem(arr, at - 1)
    a == elem(arr, at - 2) and consonant?(a)
  end

  defp ends_doubled?(_arr, _at), do: false

  # True when two letters spell one sound and must not be split (@digraphs).
  defp digraph?(a, b), do: <<lower(a), lower(b)>> in @digraphs

  # Letter classification for the rules. "y" counts as a vowel; anything that
  # is not a letter is neither a vowel nor a consonant.
  defp vowel?(c), do: lower(c) in @vowels
  defp consonant?(c), do: lower(c) in ?a..?z and lower(c) not in @vowels

  # --- Line breaking --------------------------------------------------------

  @doc """
  Break `text` into lines that each fit within `max_width`.

  Greedy: fill a line until the next word will not fit, then try to break
  that word so part of it still fits, and otherwise start a new line. A word
  that already contains a hyphen breaks after it; any other word breaks at a
  point from `hyphenation_points/1`, with a hyphen added. A word too long for
  an empty line is split at the last character that fits, with a hyphen, and
  whatever is left over is wrapped the same way, so no line is wider than
  `max_width` - unless `max_width` cannot hold even one character and a
  hyphen.

  Options:

    * `:hyphenate` - default `true`. With `false`, words move whole.
    * `:hyphen` - the character to append at a break, default `?-`.
    * `:break_capitalized` - default `false`. Capitalized words are left whole,
      because in news copy they are nearly always proper nouns and a broken
      name ("Miche-lle") reads worse than a short line.

  Widths are measured with `text_width/2`, so `max_width` is in the same units
  as `char_width`.

      iex> NaplpsText.wrap("hello world", 6, 1000)
      ["hello world"]
  """
  @spec wrap(String.t(), number(), number(), keyword()) :: [String.t()]
  def wrap(text, char_width, max_width, opts \\ []) when is_binary(text) do
    hyphenate? = Keyword.get(opts, :hyphenate, true)
    hyphen = Keyword.get(opts, :hyphen, ?-)
    caps? = Keyword.get(opts, :break_capitalized, false)

    # Work word by word. Any run of whitespace, including newlines, separates
    # words, so the input's own line breaks are not kept.
    #
    # The accumulator is {finished lines, newest first; the line being
    # filled}. place/7 decides where each word goes and returns the new
    # accumulator.
    text
    |> String.split(~r/\s+/, trim: true)
    |> Enum.reduce({[], ""}, fn word, {lines, current} ->
      # Break this word (at its own hyphen or a hyphenation point) only if
      # hyphenation is on and, unless the caller allowed it, the word is not
      # capitalized.
      may_break? = hyphenate? and (caps? or not capitalized?(word))
      place(word, current, lines, char_width, max_width, may_break?, hyphen)
    end)
    # Close the last line, unless it is empty, and restore reading order.
    |> then(fn {lines, current} ->
      Enum.reverse(if current == "", do: lines, else: [current | lines])
    end)
  end

  # Put one word onto the lines so far, and return the updated
  # {finished lines, current line}.
  #
  # `current` is the line being filled ("" when empty), `lines` the finished
  # lines newest first, `cw` the cell width and `max` the line width. With
  # `hyphenate?` false the word is never split at a break point, but a word
  # too wide for any line is still hard-split.
  defp place(word, current, lines, cw, max, hyphenate?, hyphen) do
    # The current line with this word added, separated by a space.
    candidate = if current == "", do: word, else: current <> " " <> word

    # If that still fits, the word simply joins the current line. Otherwise
    # try, in order: split the word so its first part ends this line; move it
    # whole to a new line; and, if even a line to itself is too narrow,
    # hard-split it.
    if text_width(cw, candidate) <= max do
      {lines, candidate}
    else
      split = if hyphenate?, do: split_word(word, current, cw, max, hyphen), else: nil

      case {split, current} do
        # Part of the word fits after what is already on the line: close that
        # line, then wrap the rest of the word on a fresh one. The rest may
        # still be too wide, so it goes through place/7 like any other word.
        {{head, tail}, _} ->
          place(tail, "", [head | lines], cw, max, hyphenate?, hyphen)

        # Nothing fits alongside the current line: close it and retry the word
        # on a fresh one.
        {nil, current} when current != "" ->
          place(word, "", [current | lines], cw, max, hyphenate?, hyphen)

        # Alone on an empty line and still too wide: hard-split so it cannot
        # overflow the field, then wrap what is left. Each pass takes at least
        # one character off the word, so this always finishes.
        {nil, _} ->
          {head, tail} = hard_split(word, cw, max, hyphen)
          place(tail, "", [head | lines], cw, max, hyphenate?, hyphen)
      end
    end
  end

  # True for a word with a capital first letter and at least one lowercase
  # letter after it ("Michelle"). Acronyms such as "NASA" are false here, and
  # hyphenation_points/1 refuses them separately.
  defp capitalized?(<<c, rest::binary>>) when c >= ?A and c <= ?Z,
    do: rest =~ ~r/[a-z]/

  defp capitalized?(_word), do: false

  # Split `word` at its latest break point that still lets the first part fit
  # after what is already on the current line.
  #
  # Break points are the word's own hyphens, where nothing is added, and the
  # positions from hyphenation_points/1, where `hyphen` is added. A word has
  # one kind or the other: hyphenation_points/1 offers nothing for a word that
  # already contains a hyphen.
  #
  # Returns {finished line, remainder}. The finished line is the current line
  # with the first part appended; the remainder is wrapped next. Returns nil
  # when no point fits, including when the word has no points.
  defp split_word(word, current, cw, max, hyphen) do
    prefix = if current == "", do: "", else: current <> " "

    # Each point paired with what to append at it: nothing after an existing
    # hyphen, `hyphen` anywhere else.
    points =
      Enum.map(existing_hyphen_points(word), &{&1, ""}) ++
        Enum.map(hyphenation_points(word), &{&1, <<hyphen>>})

    # Latest point first, so the first one that fits keeps as much of the word
    # on this line as possible.
    points
    |> Enum.sort(:desc)
    |> Enum.find_value(fn {at, mark} ->
      line = prefix <> String.slice(word, 0, at) <> mark

      if text_width(cw, line) <= max do
        {line, String.slice(word, at, String.length(word) - at)}
      end
    end)
  end

  # Positions just after each hyphen already in the word, where it can break
  # without adding anything: "long-term" gives 5 ("long-" / "term"). A hyphen
  # at either end of the word is not a break.
  defp existing_hyphen_points(word) do
    len = String.length(word)

    for {"-", i} <- word |> String.graphemes() |> Enum.with_index(),
        i > 0,
        i < len - 1,
        do: i + 1
  end

  # Last resort for a word too wide for a line by itself. Returns
  # {first line, remainder}; place/7 wraps the remainder in turn.
  #
  # If the word has a hyphen of its own that leaves a first part short enough,
  # cut there and add nothing, so "long-term" becomes "long-" / "term" rather
  # than gaining a second hyphen. This path is reached even when the word may
  # not be broken otherwise (hyphenation off, or a capitalized name), because
  # the word does not fit on any line.
  #
  # Otherwise cut after the most characters that fit with `hyphen` appended,
  # ignoring the hyphenation rules.
  defp hard_split(word, cw, max, hyphen) do
    len = String.length(word)

    at_own_hyphen =
      word
      |> existing_hyphen_points()
      |> Enum.reverse()
      |> Enum.find(&(text_width(cw, String.slice(word, 0, &1)) <= max))

    if at_own_hyphen do
      {String.slice(word, 0, at_own_hyphen),
       String.slice(word, at_own_hyphen, len - at_own_hyphen)}
    else
      # Try lengths from longest to shortest and keep the first that fits. If
      # none fits (the line is narrower than one character plus a hyphen),
      # take one character anyway so the word always gets shorter.
      take =
        Enum.find((len - 1)..1//-1, 1, fn n ->
          text_width(cw, String.slice(word, 0, n) <> <<hyphen>>) <= max
        end)

      {String.slice(word, 0, take) <> <<hyphen>>, String.slice(word, take, len - take)}
    end
  end
end
