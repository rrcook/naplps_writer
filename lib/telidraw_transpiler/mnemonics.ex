defmodule TelidrawTranspiler.Mnemonics do
  @moduledoc """
  Resolves UPPERCASE Telidraw mnemonics to their NAPLPS opcode.

  Telidraw distinguishes lowercase keywords (`domain 1 3 2` — high-level, args are
  *semantic values* re-encoded into operand bytes) from UPPERCASE mnemonics
  (`DOMAIN 200 192 192 201` — raw pass-through, args are *literal operand bytes*).
  The lexer keeps the lowercase keyword table case-sensitive, so any other casing
  falls through to an identifier token. This module is the identifier→opcode
  resolver that the parser consults for those identifier tokens.

  Two namespaces are matched (both case-insensitively), a port of
  `NAPLPS.CommandRegistry`:

    * **ANSI X3.110 C0 mnemonics** — fixed per-opcode names for control codes
      `0x00`–`0x1F` (`NUL`, `CAN`, `ESC`, `NSR`, …).
    * **Registry kebab-names** — each PDI command's descriptor name, spaces
      replaced by `-` (`POLYGON-SET-FILLED`, `DOMAIN`, `LINE-RELATIVE`, …).

  ANSI mnemonics are tried first, then kebab-names. The kebab table only carries
  the 8-bit opcode (`>= 0xA0`) per the registry's "prefer 8-bit variant" rule.
  """

  # ANSI X3.110 C0 mnemonics (0x00-0x1F). Keys are uppercase.
  @ansi %{
    "NUL" => 0x00,
    "SOH" => 0x01,
    "STX" => 0x02,
    "ETX" => 0x03,
    "EOT" => 0x04,
    "ENQ" => 0x05,
    "ACK" => 0x06,
    "BEL" => 0x07,
    "APB" => 0x08,
    "APF" => 0x09,
    "APD" => 0x0A,
    "APU" => 0x0B,
    "CS" => 0x0C,
    "APR" => 0x0D,
    "SO" => 0x0E,
    "SI" => 0x0F,
    "DLE" => 0x10,
    "DC1" => 0x11,
    "DC2" => 0x12,
    "DC3" => 0x13,
    "DC4" => 0x14,
    "NAK" => 0x15,
    "SYN" => 0x16,
    "ETB" => 0x17,
    "CAN" => 0x18,
    "SS2" => 0x19,
    "SDC" => 0x1A,
    "ESC" => 0x1B,
    "APS" => 0x1C,
    "SS3" => 0x1D,
    "APH" => 0x1E,
    "NSR" => 0x1F
  }

  # Registry kebab-names for the General PDI set (base 0xA0). Each is the C#
  # command descriptor's `Name` with spaces replaced by `-`, uppercased.
  @kebab %{
    "RESET" => 0xA0,
    "DOMAIN" => 0xA1,
    "TEXT" => 0xA2,
    "TEXTURE" => 0xA3,
    "POINT-SET-ABSOLUTE" => 0xA4,
    "POINT-SET-RELATIVE" => 0xA5,
    "POINT-ABSOLUTE" => 0xA6,
    "POINT-RELATIVE" => 0xA7,
    "LINE-ABSOLUTE" => 0xA8,
    "LINE-RELATIVE" => 0xA9,
    "LINE-SET-ABSOLUTE" => 0xAA,
    "LINE-SET-RELATIVE" => 0xAB,
    "ARC-OUTLINED" => 0xAC,
    "ARC-FILLED" => 0xAD,
    "ARC-SET-OUTLINED" => 0xAE,
    "ARC-SET-FILLED" => 0xAF,
    "RECTANGLE-OUTLINED" => 0xB0,
    "RECTANGLE-FILLED" => 0xB1,
    "RECTANGLE-SET-OUTLINED" => 0xB2,
    "RECTANGLE-SET-FILLED" => 0xB3,
    "POLYGON-OUTLINED" => 0xB4,
    "POLYGON-FILLED" => 0xB5,
    "POLYGON-SET-OUTLINED" => 0xB6,
    "POLYGON-SET-FILLED" => 0xB7,
    "INCREMENTAL-FIELD" => 0xB8,
    "INCREMENTAL-POINT" => 0xB9,
    "INCREMENTAL-LINE" => 0xBA,
    "INCREMENTAL-POLYGON-FILLED" => 0xBB,
    "SET-COLOR" => 0xBC,
    "WAIT" => 0xBD,
    "SELECT-COLOR" => 0xBE,
    "BLINK" => 0xBF
  }

  @doc """
  Resolve a mnemonic to its opcode byte, or `nil` if it is not a known mnemonic
  (i.e. it is a user-defined proc/variable). Matching is case-insensitive; ANSI
  C0 mnemonics take priority over kebab-names.
  """
  @spec resolve(String.t()) :: byte() | nil
  def resolve(text) when is_binary(text) do
    up = String.upcase(text)
    Map.get(@ansi, up) || Map.get(@kebab, up)
  end
end
