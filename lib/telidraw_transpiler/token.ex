defmodule TelidrawTranspiler.Token do
  @moduledoc """
  A single lexical token produced by `TelidrawTranspiler.Lexer`.

  `num` carries the numeric value for `:int`, `:float` and `:fraction` tokens
  (the numerator, for fractions); `num2` carries the denominator for `:fraction`.
  """
  defstruct [:kind, :lexeme, :line, :col, num: nil, num2: nil]

  @type t :: %__MODULE__{
          kind: atom(),
          lexeme: String.t(),
          line: non_neg_integer(),
          col: non_neg_integer(),
          num: number() | nil,
          num2: integer() | nil
        }
end
