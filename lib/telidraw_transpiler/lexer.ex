defmodule TelidrawTranspiler.Lexer do
  @moduledoc """
  Hand-rolled Telidraw lexer, a port of `NAPLPS.Telidraw.Lexer`.

  Single pass over the source; produces a flat list of `TelidrawTranspiler.Token`
  terminated by an `:eof` token, plus a list of diagnostics. Lexical errors are
  collected rather than raised so the parser can still run.
  """

  alias TelidrawTranspiler.Token

  @keywords %{
    # Structural
    "proc" => :proc,
    "with" => :with,
    "repeat" => :repeat,
    "for" => :for,
    "in" => :in,
    "if" => :if,
    "else" => :else,
    "palette" => :palette,
    "let" => :let,

    # Drawing verbs
    "move" => :move,
    "move-rel" => :move_rel,
    "goto" => :goto,
    "line" => :line,
    "line-rel" => :line_rel,
    "line-set" => :line_set,
    "line-set-rel" => :line_set_rel,
    "rect" => :rect,
    "rect-outline" => :rect_outline,
    "rect-set" => :rect_set,
    "rect-set-outline" => :rect_set_outline,
    "arc" => :arc,
    "arc-outline" => :arc_outline,
    "arc-set" => :arc_set,
    "arc-set-outline" => :arc_set_outline,
    "polygon" => :polygon,
    "polygon-outline" => :poly_outline,
    "polygon-set" => :poly_set,
    "polygon-set-outline" => :poly_set_outline,
    "poly" => :polygon,
    "poly-outline" => :poly_outline,
    "poly-set" => :poly_set,
    "poly-set-outline" => :poly_set_outline,
    "point" => :point,
    "point-rel" => :point_rel,
    "text" => :text,
    "color" => :color,
    "set-color" => :set_color,
    "texture" => :texture,
    "domain" => :domain,
    "blink" => :blink,
    "wait" => :wait,
    "reset" => :reset,
    "nsr" => :nsr,
    "drcs" => :drcs,
    "field" => :field,
    "scribble" => :scribble,
    "bitmap" => :bitmap,
    "close" => :close,
    "raw" => :raw
  }

  @doc "Tokenize `source`, returning `{tokens, diagnostics}`."
  @spec tokenize(String.t()) :: {[Token.t()], [map()]}
  def tokenize(source) when is_binary(source) do
    scan(String.to_charlist(source), 1, 1, [], [])
  end

  # ---- main scan loop ------------------------------------------------------

  defp scan([], line, col, toks, diags) do
    toks = [%Token{kind: :eof, lexeme: "", line: line, col: col} | toks]
    {Enum.reverse(toks), Enum.reverse(diags)}
  end

  # whitespace
  defp scan([c | rest], line, col, toks, diags) when c in [?\s, ?\t, ?\r],
    do: scan(rest, line, col + 1, toks, diags)

  defp scan([?\n | rest], line, _col, toks, diags),
    do: scan(rest, line + 1, 1, toks, diags)

  # line comment
  defp scan([?/, ?/ | rest], line, col, toks, diags) do
    rest = Enum.drop_while(rest, &(&1 != ?\n))
    scan(rest, line, col, toks, diags)
  end

  # block comment /* ... */  (documented in the Telidraw reference)
  defp scan([?/, ?* | rest], line, col, toks, diags) do
    {rest, line, col} = skip_block_comment(rest, line, col + 2)
    scan(rest, line, col, toks, diags)
  end

  defp scan([?/ | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :slash, "/", line, col), diags)

  # directive #name
  defp scan([?# | rest], line, col, toks, diags) do
    {name, rest} = take_while(rest, &ident_part?/1)
    tok = %Token{kind: :directive, lexeme: List.to_string(name), line: line, col: col}
    scan(rest, line, col + 1 + length(name), [tok | toks], diags)
  end

  # single-char punctuation / operators
  defp scan([?( | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :lparen, "(", line, col), diags)

  defp scan([?) | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :rparen, ")", line, col), diags)

  defp scan([?{ | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :lbrace, "{", line, col), diags)

  defp scan([?} | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :rbrace, "}", line, col), diags)

  defp scan([?[ | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :lbracket, "[", line, col), diags)

  defp scan([?] | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :rbracket, "]", line, col), diags)

  defp scan([?, | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :comma, ",", line, col), diags)

  defp scan([?: | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :colon, ":", line, col), diags)

  defp scan([?; | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :semicolon, ";", line, col), diags)

  defp scan([?+ | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :plus, "+", line, col), diags)

  defp scan([?* | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :star, "*", line, col), diags)

  defp scan([?% | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :percent, "%", line, col), diags)

  defp scan([?= | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :equals, "=", line, col), diags)

  defp scan([?@ | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :at, "@", line, col), diags)

  # minus: negative number if a digit follows, else the subtraction operator
  defp scan([?-, d | _] = chars, line, col, toks, diags) when d >= ?0 and d <= ?9 do
    [_ | rest] = chars
    read_number(rest, [?-], false, line, col, toks, diags)
  end

  defp scan([?- | rest], line, col, toks, diags),
    do: scan(rest, line, col + 1, emit(toks, :minus, "-", line, col), diags)

  # dot: ".." range, ".5" leading-dot float, else error
  defp scan([?., ?. | rest], line, col, toks, diags),
    do: scan(rest, line, col + 2, emit(toks, :dotdot, "..", line, col), diags)

  defp scan([?., d | rest], line, col, toks, diags) when d >= ?0 and d <= ?9,
    do: read_number([d | rest], [?0, ?.], true, line, col, toks, diags)

  defp scan([?. | rest], line, col, toks, diags) do
    diags = [diag(:error, line, col, "Unexpected '.' (did you mean '..'?)") | diags]
    scan(rest, line, col + 1, toks, diags)
  end

  # string literal
  defp scan([?" | rest], line, col, toks, diags),
    do: read_string(rest, [], line, col, line, col + 1, toks, diags)

  # numbers
  defp scan([d | rest], line, col, toks, diags) when d >= ?0 and d <= ?9,
    do: read_number(rest, [d], false, line, col, toks, diags)

  # identifiers / keywords
  defp scan([c | _] = chars, line, col, toks, diags) do
    cond do
      ident_start?(c) ->
        read_identifier(chars, line, col, toks, diags)

      true ->
        [_ | rest] = chars
        diags = [diag(:error, line, col, "Unexpected character '#{<<c::utf8>>}'") | diags]
        scan(rest, line, col + 1, toks, diags)
    end
  end

  # ---- block comments ------------------------------------------------------

  defp skip_block_comment([], line, col), do: {[], line, col}
  defp skip_block_comment([?*, ?/ | rest], line, col), do: {rest, line, col + 2}
  defp skip_block_comment([?\n | rest], line, _col), do: skip_block_comment(rest, line + 1, 1)
  defp skip_block_comment([_ | rest], line, col), do: skip_block_comment(rest, line, col + 1)

  # ---- strings -------------------------------------------------------------

  defp read_string([], acc, sline, scol, _line, _col, toks, diags) do
    diags = [diag(:error, sline, scol, "Unterminated string literal") | diags]
    tok = %Token{kind: :string, lexeme: rev_to_string(acc), line: sline, col: scol}
    scan([], sline, scol, [tok | toks], diags)
  end

  defp read_string([?" | rest], acc, sline, scol, line, col, toks, diags) do
    tok = %Token{kind: :string, lexeme: rev_to_string(acc), line: sline, col: scol}
    scan(rest, line, col + 1, [tok | toks], diags)
  end

  defp read_string([?\\, esc | rest], acc, sline, scol, line, col, toks, diags) do
    ch =
      case esc do
        ?n -> ?\n
        ?t -> ?\t
        ?r -> ?\r
        ?\\ -> ?\\
        ?" -> ?"
        other -> other
      end

    read_string(rest, [ch | acc], sline, scol, line, col + 2, toks, diags)
  end

  defp read_string([?\n | rest], acc, sline, scol, line, _col, toks, diags),
    do: read_string(rest, [?\n | acc], sline, scol, line + 1, 1, toks, diags)

  defp read_string([c | rest], acc, sline, scol, line, col, toks, diags),
    do: read_string(rest, [c | acc], sline, scol, line, col + 1, toks, diags)

  # ---- numbers -------------------------------------------------------------
  #
  # `acc` holds the reversed digits read so far (including a leading '-' or the
  # synthesized "0." for leading-dot floats). Mirrors the C# ScanNumber: read
  # digits and at most one decimal point, then optionally a `/denominator` to
  # form a fraction literal.

  defp read_number(chars, acc, has_dot, line, col, toks, diags) do
    {acc, has_dot, rest} = read_number_body(chars, acc, has_dot)
    num_text = rev_to_string(acc)

    case rest do
      # Fraction N/M — only when a digit immediately follows the slash.
      [?/, d | _] when not has_dot and d >= ?0 and d <= ?9 ->
        [_ | after_slash] = rest
        {denom_chars, rest2} = take_while(after_slash, &digit?/1)
        denom_text = List.to_string(denom_chars)

        with {n, ""} <- Integer.parse(num_text),
             {dn, ""} <- Integer.parse(denom_text),
             true <- dn != 0 do
          lex = "#{n}/#{dn}"

          tok = %Token{
            kind: :fraction,
            lexeme: lex,
            line: line,
            col: col,
            num: n,
            num2: dn
          }

          scan(rest2, line, col + String.length(lex), [tok | toks], diags)
        else
          _ ->
            diags = [
              diag(:error, line, col, "Invalid fraction '#{num_text}/#{denom_text}'") | diags
            ]

            scan(rest2, line, col, toks, diags)
        end

      _ ->
        cond do
          has_dot ->
            case Float.parse(num_text) do
              {f, ""} ->
                tok = %Token{kind: :float, lexeme: num_text, line: line, col: col, num: f}
                scan(rest, line, col + String.length(num_text), [tok | toks], diags)

              _ ->
                diags = [diag(:error, line, col, "Invalid float literal '#{num_text}'") | diags]
                scan(rest, line, col, toks, diags)
            end

          true ->
            case Integer.parse(num_text) do
              {i, ""} ->
                tok = %Token{kind: :int, lexeme: num_text, line: line, col: col, num: i}
                scan(rest, line, col + String.length(num_text), [tok | toks], diags)

              _ ->
                diags = [diag(:error, line, col, "Invalid integer literal '#{num_text}'") | diags]
                scan(rest, line, col, toks, diags)
            end
        end
    end
  end

  defp read_number_body([d | rest], acc, has_dot) when d >= ?0 and d <= ?9,
    do: read_number_body(rest, [d | acc], has_dot)

  # A single '.' is part of the number only when we haven't already seen one and
  # the next char is not another '.' (which would be the `..` range operator).
  defp read_number_body([?., c | _] = chars, acc, false) when c != ?. do
    [_ | rest] = chars
    read_number_body(rest, [?. | acc], true)
  end

  defp read_number_body([?.], acc, false), do: {[?. | acc], true, []}

  defp read_number_body(rest, acc, has_dot), do: {acc, has_dot, rest}

  # ---- identifiers ---------------------------------------------------------

  defp read_identifier(chars, line, col, toks, diags) do
    {ident, rest} = read_ident_chars(chars, [])
    text = rev_to_string(ident)
    kind = Map.get(@keywords, text, :identifier)
    tok = %Token{kind: kind, lexeme: text, line: line, col: col}
    scan(rest, line, col + String.length(text), [tok | toks], diags)
  end

  # Smart hyphen: '-' joins an identifier only when flanked by letters
  # (e.g. `point-rel`, `line-set-rel`). Otherwise it's the minus operator.
  defp read_ident_chars([?-, n | rest], [prev | _] = acc)
       when ((prev >= ?a and prev <= ?z) or (prev >= ?A and prev <= ?Z)) and
              ((n >= ?a and n <= ?z) or (n >= ?A and n <= ?Z)),
       do: read_ident_chars(rest, [n, ?- | acc])

  defp read_ident_chars([c | rest], acc)
       when (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z) or (c >= ?0 and c <= ?9) or c == ?_,
       do: read_ident_chars(rest, [c | acc])

  defp read_ident_chars(chars, acc), do: {acc, chars}

  # ---- shared helpers ------------------------------------------------------

  defp take_while(chars, pred), do: do_take_while(chars, pred, [])

  defp do_take_while([c | rest], pred, acc) do
    if pred.(c), do: do_take_while(rest, pred, [c | acc]), else: {Enum.reverse(acc), [c | rest]}
  end

  defp do_take_while([], _pred, acc), do: {Enum.reverse(acc), []}

  defp emit(toks, kind, lexeme, line, col),
    do: [%Token{kind: kind, lexeme: lexeme, line: line, col: col} | toks]

  defp diag(sev, line, col, msg),
    do: %{severity: sev, line: line, column: col, message: msg}

  defp rev_to_string(acc), do: acc |> Enum.reverse() |> List.to_string()

  defp digit?(c), do: c >= ?0 and c <= ?9
  defp letter?(c), do: (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z)
  defp ident_start?(c), do: letter?(c) or c == ?_
  defp ident_part?(c), do: letter?(c) or digit?(c) or c == ?_
end
