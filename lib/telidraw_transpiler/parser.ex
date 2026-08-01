defmodule TelidrawTranspiler.Parser do
  @moduledoc """
  Recursive-descent Telidraw parser, a port of `NAPLPS.Telidraw.Parser`.

  Consumes the token list from `TelidrawTranspiler.Lexer` and produces an AST
  (plain maps tagged with a `:type` key). Expression parsing uses Pratt-style
  precedence climbing. Leading `#directives` are hoisted into the program header.

  On the first syntax error the parser aborts and returns `{:error, diagnostics}`;
  well-formed input returns `{:ok, program, diagnostics}`.
  """

  import Bitwise
  alias TelidrawTranspiler.{Token, Mnemonics}

  # Drawing/attribute verbs that begin a command statement.
  @command_verbs MapSet.new([
                   :move,
                   :move_rel,
                   :goto,
                   :line,
                   :line_rel,
                   :line_set,
                   :line_set_rel,
                   :rect,
                   :rect_outline,
                   :rect_set,
                   :rect_set_outline,
                   :arc,
                   :arc_outline,
                   :arc_set,
                   :arc_set_outline,
                   :polygon,
                   :poly_outline,
                   :poly_set,
                   :poly_set_outline,
                   :point,
                   :point_rel,
                   :text,
                   :color,
                   :set_color,
                   :texture,
                   :domain,
                   :blink,
                   :wait,
                   :reset,
                   :nsr,
                   :drcs,
                   :field,
                   :scribble,
                   :bitmap,
                   :close,
                   :raw
                 ])

  @with_attributes MapSet.new([:color, :texture, :domain])

  @expr_start MapSet.new([:int, :float, :fraction, :string, :identifier, :lparen, :minus, :plus])

  @doc "Parse `tokens`, returning `{:ok, program, diags}` or `{:error, diags}`."
  @spec parse([Token.t()]) :: {:ok, map(), [map()]} | {:error, [map()]}
  def parse(tokens) do
    {directives, rest} = parse_leading_directives(tokens, [])
    {statements, _rest} = parse_statements(rest, [])

    program = %{
      type: :program,
      directives: directives,
      statements: statements,
      line: 1,
      col: 1
    }

    {:ok, program, []}
  catch
    {:telidraw_parse_error, diag} -> {:error, [diag]}
  end

  # ---- top-level -----------------------------------------------------------

  defp parse_leading_directives([%Token{kind: :directive} | _] = toks, acc) do
    {dir, rest} = parse_directive(toks)
    parse_leading_directives(rest, [dir | acc])
  end

  defp parse_leading_directives(toks, acc), do: {Enum.reverse(acc), toks}

  defp parse_statements([%Token{kind: :eof} | _] = toks, acc), do: {Enum.reverse(acc), toks}

  defp parse_statements(toks, acc) do
    {stmt, rest} = parse_statement(toks)
    parse_statements(rest, [stmt | acc])
  end

  # ---- statements ----------------------------------------------------------

  defp parse_statement([%Token{kind: kind} = tok | _] = toks) do
    cond do
      kind == :proc -> parse_proc_decl(toks, false)
      kind == :at -> parse_macro_proc(toks)
      kind == :with -> parse_with_block(toks)
      kind == :repeat -> parse_repeat(toks)
      kind == :for -> parse_for_loop(toks)
      kind == :if -> parse_if(toks)
      kind == :palette -> parse_palette_alias(toks)
      kind == :let -> parse_let(toks)
      kind == :directive -> parse_directive(toks)
      kind == :raw -> parse_raw_statement(toks)
      MapSet.member?(@command_verbs, kind) -> parse_command_call(toks)
      kind == :identifier -> parse_identifier_statement(toks, tok)
      true -> throw_error(tok, "Expected a statement, got '#{tok.lexeme}'")
    end
  end

  defp parse_macro_proc(toks) do
    {at, rest} = expect(toks, :at)

    case rest do
      [%Token{kind: :identifier, lexeme: "macro"} | rest2] ->
        {_, rest3} = expect(rest2, :proc)
        parse_proc_decl_after_keyword(rest3, true, at.line, at.col)

      _ ->
        throw_error(at, "Expected '@macro' before 'proc'")
    end
  end

  defp parse_proc_decl(toks, as_macro) do
    {proc, rest} = expect(toks, :proc)
    parse_proc_decl_after_keyword(rest, as_macro, proc.line, proc.col)
  end

  defp parse_proc_decl_after_keyword(toks, as_macro, line, col) do
    {name_tok, rest} = expect(toks, :identifier)
    {_, rest} = expect(rest, :lparen)
    {params, rest} = parse_params(rest)
    {_, rest} = expect(rest, :rparen)
    {body, rest} = parse_block(rest)

    node = %{
      type: :proc_decl,
      name: name_tok.lexeme,
      params: params,
      body: body,
      macro: as_macro,
      line: line,
      col: col
    }

    {node, rest}
  end

  defp parse_params([%Token{kind: :rparen} | _] = toks), do: {[], toks}

  defp parse_params(toks) do
    {p, rest} = expect(toks, :identifier)
    parse_params_tail(rest, [p.lexeme])
  end

  defp parse_params_tail([%Token{kind: :comma} | rest], acc) do
    {p, rest} = expect(rest, :identifier)
    parse_params_tail(rest, [p.lexeme | acc])
  end

  defp parse_params_tail(toks, acc), do: {Enum.reverse(acc), toks}

  defp parse_with_block(toks) do
    {with_tok, rest} = expect(toks, :with)
    {attr_tok, rest} = advance(rest)

    unless MapSet.member?(@with_attributes, attr_tok.kind) do
      throw_error(attr_tok, "'with' expects one of color, texture, domain")
    end

    {args, rest} = parse_exprs_until_block(rest, [])
    {body, rest} = parse_block(rest)

    node = %{
      type: :with,
      attr: attr_tok.kind,
      attr_args: args,
      body: body,
      line: with_tok.line,
      col: with_tok.col
    }

    {node, rest}
  end

  defp parse_exprs_until_block([%Token{kind: :lbrace} | _] = toks, acc),
    do: {Enum.reverse(acc), toks}

  defp parse_exprs_until_block([%Token{kind: :eof} | _] = toks, acc),
    do: {Enum.reverse(acc), toks}

  defp parse_exprs_until_block(toks, acc) do
    {expr, rest} = parse_expression(toks)
    parse_exprs_until_block(rest, [expr | acc])
  end

  defp parse_repeat(toks) do
    {tok, rest} = expect(toks, :repeat)
    {count, rest} = parse_expression(rest)
    {body, rest} = parse_block(rest)
    {%{type: :repeat, count: count, body: body, line: tok.line, col: tok.col}, rest}
  end

  defp parse_for_loop(toks) do
    {tok, rest} = expect(toks, :for)
    {var, rest} = expect(rest, :identifier)
    {_, rest} = expect(rest, :in)
    {from, rest} = parse_expression(rest)
    {_, rest} = expect(rest, :dotdot)
    {to, rest} = parse_expression(rest)
    {body, rest} = parse_block(rest)

    node = %{
      type: :for,
      var: var.lexeme,
      from: from,
      to: to,
      body: body,
      line: tok.line,
      col: tok.col
    }

    {node, rest}
  end

  defp parse_if(toks) do
    {tok, rest} = expect(toks, :if)
    {cond_expr, rest} = parse_expression(rest)
    {then_body, rest} = parse_block(rest)

    {else_body, rest} =
      case rest do
        [%Token{kind: :else} | rest2] ->
          {body, rest3} = parse_block(rest2)
          {body, rest3}

        _ ->
          {nil, rest}
      end

    node = %{
      type: :if,
      cond: cond_expr,
      then: then_body,
      else: else_body,
      line: tok.line,
      col: tok.col
    }

    {node, rest}
  end

  defp parse_palette_alias(toks) do
    {tok, rest} = expect(toks, :palette)
    {name, rest} = expect(rest, :identifier)
    {_, rest} = expect(rest, :equals)
    {value, rest} = parse_expression(rest)
    {%{type: :palette_alias, name: name.lexeme, value: value, line: tok.line, col: tok.col}, rest}
  end

  defp parse_let(toks) do
    {tok, rest} = expect(toks, :let)
    {name, rest} = expect(rest, :identifier)
    {_, rest} = expect(rest, :equals)
    {value, rest} = parse_expression(rest)
    {%{type: :let, name: name.lexeme, value: value, line: tok.line, col: tok.col}, rest}
  end

  defp parse_directive(toks) do
    {tok, rest} = expect(toks, :directive)
    {args, rest} = parse_directive_args(rest, tok.line, [])
    {%{type: :directive, name: tok.lexeme, args: args, line: tok.line, col: tok.col}, rest}
  end

  # Directive args live on the same source line as the directive keyword.
  defp parse_directive_args([%Token{kind: kind, line: line} | _] = toks, dir_line, acc) do
    if kind != :directive and MapSet.member?(@expr_start, kind) and line == dir_line do
      {expr, rest} = parse_expression(toks)
      parse_directive_args(rest, dir_line, [expr | acc])
    else
      {Enum.reverse(acc), toks}
    end
  end

  # `raw <opcode> <byte> ...` — literal form: the first number IS the opcode and
  # all bytes are preserved exactly (never bit-adjusted). `logical: false`.
  defp parse_raw_statement(toks) do
    {tok, rest} = expect(toks, :raw)
    {bytes, rest} = parse_raw_bytes(rest, [])
    {%{type: :raw, bytes: bytes, logical: false, line: tok.line, col: tok.col}, rest}
  end

  # An identifier statement is either a raw mnemonic command (if the word resolves
  # to a NAPLPS opcode — `DOMAIN`, `NSR`, `POLYGON-SET-FILLED`, ...) or a call to a
  # user-defined proc. Case-insensitive; lowercase keywords never reach here (the
  # lexer already claimed them as keyword tokens).
  defp parse_identifier_statement(toks, tok) do
    case Mnemonics.resolve(tok.lexeme) do
      nil -> parse_proc_call(toks)
      opcode -> parse_mnemonic_statement(toks, opcode)
    end
  end

  # `MNEMONIC b1 b2 ...` — the mnemonic supplies the opcode; following numeric
  # literals become operand bytes verbatim. `logical: true` marks the canonical
  # 8-bit (0xA0+) opcode so 7-bit emission can bit-adjust it.
  defp parse_mnemonic_statement(toks, opcode) do
    {tok, rest} = advance(toks)
    {operands, rest} = parse_raw_bytes(rest, [])
    {%{type: :raw, bytes: [opcode | operands], logical: true, line: tok.line, col: tok.col}, rest}
  end

  defp parse_raw_bytes([%Token{kind: kind} | _] = toks, acc)
       when kind in [:int, :float, :fraction, :minus, :plus] do
    {expr, rest} = parse_expression(toks)

    case expr do
      %{type: :num, value: v} -> parse_raw_bytes(rest, [trunc(v) &&& 0xFF | acc])
      _ -> parse_raw_bytes(rest, acc)
    end
  end

  defp parse_raw_bytes(toks, acc), do: {Enum.reverse(acc), toks}

  defp parse_command_call(toks) do
    {tok, rest} = advance(toks)
    {args, rest} = parse_command_args(rest, [])
    {%{type: :command, kind: tok.kind, args: args, line: tok.line, col: tok.col}, rest}
  end

  defp parse_command_args([%Token{kind: kind} = tok | _] = toks, acc) do
    # Stop at an identifier that resolves to a mnemonic: it starts the NEXT
    # statement (e.g. `move 84/256 92/256` then `LINE-RELATIVE ...`), it is not an
    # argument. Plain identifiers (variable references) are still consumed as args.
    cond do
      kind == :identifier and Mnemonics.resolve(tok.lexeme) != nil ->
        {Enum.reverse(acc), toks}

      MapSet.member?(@expr_start, kind) ->
        {expr, rest} = parse_expression(toks)
        parse_command_args(rest, [expr | acc])

      true ->
        {Enum.reverse(acc), toks}
    end
  end

  defp parse_proc_call(toks) do
    {name, rest} = expect(toks, :identifier)
    {args, rest} = parse_command_args(rest, [])
    {%{type: :proc_call, name: name.lexeme, args: args, line: name.line, col: name.col}, rest}
  end

  defp parse_block(toks) do
    {_, rest} = expect(toks, :lbrace)
    parse_block_body(rest, [])
  end

  defp parse_block_body([%Token{kind: :rbrace} | rest], acc), do: {Enum.reverse(acc), rest}

  defp parse_block_body([%Token{kind: :eof} = tok | _], _acc),
    do: throw_error(tok, "Unexpected end of file inside a block (missing '}')")

  defp parse_block_body(toks, acc) do
    {stmt, rest} = parse_statement(toks)
    parse_block_body(rest, [stmt | acc])
  end

  # ---- expressions (Pratt precedence climbing) -----------------------------

  defp precedence(:plus), do: 10
  defp precedence(:minus), do: 10
  defp precedence(:star), do: 20
  defp precedence(:slash), do: 20
  defp precedence(:percent), do: 20
  defp precedence(_), do: 0

  defp parse_expression(toks, min_prec \\ 0) do
    {left, rest} = parse_unary(toks)
    parse_binop_loop(left, rest, min_prec)
  end

  defp parse_binop_loop(left, [%Token{kind: kind} = op | rest], min_prec) do
    prec = precedence(kind)

    if prec == 0 or prec < min_prec do
      {left, [op | rest]}
    else
      {right, rest2} = parse_expression(rest, prec + 1)

      node = %{
        type: :binop,
        op: op.lexeme,
        left: left,
        right: right,
        line: op.line,
        col: op.col
      }

      parse_binop_loop(node, rest2, min_prec)
    end
  end

  defp parse_binop_loop(left, toks, _min_prec), do: {left, toks}

  defp parse_unary([%Token{kind: kind} = op | rest]) when kind in [:minus, :plus] do
    {operand, rest2} = parse_unary(rest)
    {%{type: :unop, op: op.lexeme, operand: operand, line: op.line, col: op.col}, rest2}
  end

  defp parse_unary(toks), do: parse_primary(toks)

  defp parse_primary([%Token{kind: :int} = tok | rest]),
    do: {%{type: :num, value: tok.num, line: tok.line, col: tok.col}, rest}

  defp parse_primary([%Token{kind: :float} = tok | rest]),
    do: {%{type: :num, value: tok.num, line: tok.line, col: tok.col}, rest}

  defp parse_primary([%Token{kind: :fraction} = tok | rest]),
    do: {%{type: :frac, num: tok.num, den: tok.num2, line: tok.line, col: tok.col}, rest}

  defp parse_primary([%Token{kind: :string} = tok | rest]),
    do: {%{type: :str, value: tok.lexeme, line: tok.line, col: tok.col}, rest}

  defp parse_primary([%Token{kind: :identifier} = tok, %Token{kind: :lparen} | rest]) do
    {args, rest} = parse_call_args(rest, [])
    {_, rest} = expect(rest, :rparen)
    {%{type: :call, name: tok.lexeme, args: args, line: tok.line, col: tok.col}, rest}
  end

  defp parse_primary([%Token{kind: :identifier} = tok | rest]),
    do: {%{type: :ident, name: tok.lexeme, line: tok.line, col: tok.col}, rest}

  defp parse_primary([%Token{kind: :lparen} | rest]) do
    {inner, rest} = parse_expression(rest)
    {_, rest} = expect(rest, :rparen)
    {inner, rest}
  end

  defp parse_primary([tok | _]),
    do: throw_error(tok, "Expected an expression, got '#{tok.lexeme}'")

  defp parse_call_args([%Token{kind: :rparen} | _] = toks, _acc), do: {[], toks}

  defp parse_call_args(toks, _acc) do
    {first, rest} = parse_expression(toks)
    parse_call_args_tail(rest, [first])
  end

  defp parse_call_args_tail([%Token{kind: :comma} | rest], acc) do
    {expr, rest} = parse_expression(rest)
    parse_call_args_tail(rest, [expr | acc])
  end

  defp parse_call_args_tail(toks, acc), do: {Enum.reverse(acc), toks}

  # ---- helpers -------------------------------------------------------------

  defp advance([tok | rest]), do: {tok, rest}

  defp expect([%Token{kind: kind} = tok | rest], kind), do: {tok, rest}

  defp expect([tok | _], kind),
    do: throw_error(tok, "Expected #{kind}, got #{tok.kind}")

  defp throw_error(%Token{} = tok, msg) do
    throw(
      {:telidraw_parse_error, %{severity: :error, line: tok.line, column: tok.col, message: msg}}
    )
  end
end
