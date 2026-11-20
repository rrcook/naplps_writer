# Telidraw keyword casing: uppercase vs lowercase

**Context:** This document explains how the reference C# implementation
(`NAPLPS.Telidraw` namespace) distinguishes lowercase keywords (`domain`) from
uppercase ones (`DOMAIN`) when **reading Telidraw source and producing NAPLPS
bytes**. It's written for an Elixir port whose first pass does not recognize the
uppercase forms. Read the "What the port is probably doing wrong" and "Reader
algorithm to implement" sections first — they're the fix.

---

## 1. The core rule

Case is **not** cosmetic. Lowercase and uppercase select between two *different
encodings of the same command*, even when the word is spelled identically:

| Form          | Example            | Meaning                                                                 |
|---------------|--------------------|-------------------------------------------------------------------------|
| **lowercase** | `domain 1 3 2`     | **High-level / decoded** form. Args are *semantic values* the compiler re-encodes into operand bytes. |
| **UPPERCASE** | `DOMAIN 65 72 65`  | **Raw byte pass-through.** Args are *literal operand bytes*, emitted verbatim (guarantees byte-identical round-trip). |

So `domain 1 3 2` and `DOMAIN 65 72 65` can produce the *same* NAPLPS bytes, but
they are parsed by completely different code paths and their numbers mean
different things (semantic values vs. raw bytes).

The decompiler (NAPLPS → Telidraw) prefers the lowercase high-level form and only
falls back to the UPPERCASE raw form when a decoded form wouldn't round-trip
byte-exact. Your reader (Telidraw → NAPLPS) must therefore accept **both**.

---

## 2. Why case is the discriminator

The mechanism lives in two stages: the **lexer** classifies the token by case,
and the **parser** routes on that classification.

### Lexer (`Lexer.cs`)
- There is a single keyword table, and **every entry is lowercase**
  (`"domain"`, `"move"`, `"rect"`, `"polygon-set-outline"`, ...).
- The lookup is **case-sensitive** (C# `StringComparer.Ordinal`).
- Therefore:
  - `domain`  → matches → a **keyword token** (`TokenKind.Domain`)
  - `DOMAIN`  → misses  → a generic **identifier token** (`TokenKind.Identifier`)
  - `Domain`, `DoMaIn`  → also miss → identifier tokens

The lowercase-only, case-sensitive keyword table is the *entire* reason the two
forms can coexist without colliding. Lowercase spellings are reserved for
keywords; every other casing becomes an identifier.

### Parser (`Parser.cs`, statement dispatch)
Statement routing, in order:
1. Keyword token (e.g. `TokenKind.Domain`) → `ParseCommandCall` — **high-level
   decoded path**. Args are parsed as expressions (numbers, fractions, variables).
2. Identifier token whose text **resolves to an opcode** (see §3) →
   `ParseMnemonicStatement` — **raw byte path**. Following numeric literals become
   operand bytes verbatim.
3. Identifier token that does *not* resolve to an opcode → treated as a
   user-defined proc call / variable.

Key subtlety: the opcode resolver used in step 2 is itself **case-insensitive**,
but by the time it runs, lowercase words have already been claimed as keyword
tokens by the lexer and never reach it. Uppercase is just the *canonical* raw
spelling; `domain` (lowercase) as a raw mnemonic is impossible because the lexer
ate it as a keyword first.

---

## 3. The two uppercase/raw namespaces

An identifier is a raw-command mnemonic if it resolves via **either** table
(both looked up case-insensitively):

### (a) ANSI X3.110 C0 mnemonics — fixed per-opcode names
These are the canonical names for control (C0) opcodes `0x00`–`0x1F`. A line like
`NSR 127 79` emits opcode `0x1F` with operands `[127, 79]`.

```
0x00 NUL  0x01 SOH  0x02 STX  0x03 ETX   0x04 EOT  0x05 ENQ  0x06 ACK  0x07 BEL
0x08 APB  0x09 APF  0x0A APD  0x0B APU   0x0C CS   0x0D APR  0x0E SO   0x0F SI
0x10 DLE  0x11 DC1  0x12 DC2  0x13 DC3   0x14 DC4  0x15 NAK  0x16 SYN  0x17 ETB
0x18 CAN  0x19 SS2  0x1A SDC  0x1B ESC   0x1C APS  0x1D SS3  0x1E APH  0x1F NSR
```

### (b) Registry kebab-names — command names, uppercased with `-` for spaces
Every command descriptor has a name (e.g. `"polygon set filled"`). The raw form
is that name **kebab-cased** (`polygon-set-filled`) and matched
case-insensitively. The decompiler emits it **UPPERCASE** (`POLYGON-SET-FILLED`,
`DOMAIN`, `TEXT`, ...) so it lexes as an identifier rather than a keyword.

Resolution order in the reference resolver: try the ANSI mnemonic table first
(shorter, idiomatic for C0), then the kebab-name table. Returns "no opcode" for
identifiers that match neither (those are proc calls / variables).

When a kebab name has both a 7-bit and 8-bit opcode variant, the resolver
**prefers the 8-bit opcode (>= 0xA0)**. Bit-mode adjustment happens later at emit
time (§4), not here.

---

## 4. How the raw path emits bytes (compiler side)

`ParseMnemonicStatement` builds a raw statement node: `[opcode, b1, b2, ...]`
where `opcode` came from §3 and `b1..` are the literal numeric args (each masked
to a byte, `& 0xFF`). It is flagged **logical form** (the opcode is the canonical
8-bit base `0xA0+`).

The compiler's raw-emit (`CompileRaw`) writes those bytes **verbatim**, with one
adjustment for 7-bit transmission mode:

- If the statement is the **logical kebab form** AND 7-bit mode is active AND the
  opcode is `>= 0xA0`, the opcode's bit 7 is stripped (`opcode & 0x7F`).
- The literal `raw N N N ...` form (leading `raw` keyword, first number *is* the
  opcode) is **never** adjusted — its bytes are preserved exactly, because
  disk-honest round-trip of unhandled/ambiguous opcodes depends on it.

Operand bytes are appended unchanged. Raw statements bypass command construction
entirely (no pen movement, no state mutation via constructors), *except* that the
compiler mirrors two side effects manually so later high-level commands compute
correctly: pen position (for position-changing PDI opcodes) and `DOMAIN`'s
`sv/mv/dim` state (which changes downstream coordinate byte-widths).

There is also a third, older raw form worth supporting: `raw <opcode> <bytes...>`
— the literal keyword `raw` followed by numbers, first number is the opcode byte
itself. This is the "preserve these exact bytes" escape hatch.

---

## 5. What the port is probably doing wrong

The uppercase forms aren't recognized. The most likely causes, in order:

1. **Case-insensitive keyword matching.** If the Elixir lexer matches keywords
   case-insensitively, then `DOMAIN` becomes the `domain` *keyword* and gets
   routed to the high-level decoded path — where its args (`65 72 65`) are
   misinterpreted as semantic values, or it fails because the arg pattern doesn't
   fit. **Fix:** keywords must match **lowercase only, case-sensitively.**

2. **No raw/mnemonic statement path at all.** If the parser only knows the
   lowercase keyword commands and treats every other identifier as a proc/variable
   (or an error), then `DOMAIN`, `POLYGON-SET-FILLED`, `NSR`, `CAN`, etc. are
   unrecognized. **Fix:** add the identifier→opcode resolution (both tables in
   §3) and a raw-byte statement parser.

3. **Missing the ANSI C0 mnemonic table.** Even with kebab-name resolution,
   `NSR`/`CAN`/`ESC`/... won't resolve without the fixed table in §3(a).

4. **Not preferring the 8-bit opcode / not handling 7-bit bit-stripping**, causing
   byte mismatches even when parsing succeeds (§3 last paragraph, §4).

---

## 6. Reader algorithm to implement (Telidraw → NAPLPS)

Pseudocode for the two decision points:

```
# --- Lexer: classify an identifier-shaped word ---
def classify(word):
    if word in LOWERCASE_KEYWORDS:      # exact, case-sensitive, lowercase-only
        return keyword_token(word)
    return identifier_token(word)       # any other casing lands here

# --- Parser: dispatch a statement whose first token is `tok` ---
def parse_statement(tok):
    if is_keyword(tok):                 # move / line / domain / rect / ...
        return parse_high_level_command(tok)   # args are SEMANTIC values

    if is_identifier(tok):
        opcode = resolve_mnemonic(tok.text)     # case-insensitive; see below
        if opcode is not None:
            return parse_raw_mnemonic(tok, opcode)  # following numbers are BYTES
        return parse_proc_or_var(tok)   # user identifier

# --- Opcode resolution for an identifier (case-insensitive) ---
def resolve_mnemonic(text):
    if text (upcased) in ANSI_C0_MNEMONICS:      # NUL..NSR, table in §3(a)
        return ansi_opcode(text)
    op = kebab_name_to_opcode(text)              # "POLYGON-SET-FILLED" etc.
    return op if op != 0 else None               # prefer 8-bit (>=0xA0) variant

# --- Parse `MNEMONIC b1 b2 b3 ...` ---
def parse_raw_mnemonic(tok, opcode):
    bytes = [opcode]
    while next_token_is_numeric_literal():       # int/float/fraction, +/- sign
        bytes.append(parse_number() & 0xFF)      # STOP at first non-numeric token
    return RawStatement(bytes, logical_form=true)
```

Emission (`RawStatement` → NAPLPS bytes):

```
def emit_raw(stmt, use_7bit):
    op = stmt.bytes[0]
    if stmt.logical_form and use_7bit and op >= 0xA0:
        op = op & 0x7F                  # 7-bit transmission base
    output(op)
    for b in stmt.bytes[1:]:
        output(b)                       # operands verbatim
    # mirror side effects for downstream high-level commands:
    update_pen_if_position_command(op, operands)
    if op is DOMAIN: update_sv_mv_dim_state(operands)
```

Critical detail in the arg loop: **stop consuming args at the first non-numeric
token.** An identifier that resolves to a mnemonic is the *start of the next
statement*, not an argument to the current one. (The reference parser applies the
same "stop at a mnemonic identifier" rule to the high-level command arg loop too.)

---

## 7. Concrete examples

| Telidraw source            | Path        | NAPLPS result                                   |
|----------------------------|-------------|-------------------------------------------------|
| `domain 1 3 2`             | high-level  | DOMAIN opcode + fixed-byte encoding of sv/mv/dim |
| `DOMAIN 65 72 65`          | raw         | DOMAIN opcode + operand bytes `[65, 72, 65]`     |
| `move 128/256 96/256`      | high-level  | Point-Set-Absolute, coords encoded from fractions|
| `POLYGON-SET-FILLED 64 75` | raw         | that opcode + operand bytes `[64, 75]`           |
| `NSR 127 79`               | raw (ANSI)  | opcode `0x1F` + `[127, 79]`                       |
| `CAN`                      | raw (ANSI)  | opcode `0x18`, no operands                        |
| `raw 32 65 66`             | literal raw | opcode `0x20` (verbatim) + `[65, 66]`             |
| `myproc 1 2`               | proc call   | (not a command — user-defined)                   |

---

## 8. Port checklist

- [ ] Keyword table is **lowercase-only** and matched **case-sensitively**.
- [ ] Parser has a **raw/mnemonic statement path** for identifiers that resolve
      to an opcode.
- [ ] **ANSI C0 mnemonic table** (`NUL`..`NSR`, §3a) present, case-insensitive.
- [ ] **Kebab-name → opcode** resolution (§3b), case-insensitive, **prefers the
      8-bit (`>= 0xA0`) opcode** when both variants exist.
- [ ] Raw arg loop consumes **only numeric literals**, masks each `& 0xFF`, and
      **stops at the first non-numeric token**.
- [ ] Emit strips **bit 7 of the opcode** for the logical/kebab form when in
      **7-bit mode** and opcode `>= 0xA0`; literal `raw N ...` form is verbatim.
- [ ] Raw emission mirrors **pen position** and **`DOMAIN` sv/mv/dim state** so
      subsequent high-level commands encode correctly.
- [ ] Also accept the legacy literal form `raw <opcode> <bytes...>`.

---

## 9. Reference source map (C#)

- `NAPLPS/Telidraw/Lexer.cs` — keyword table (lowercase, `StringComparer.Ordinal`),
  identifier scanning (note the "smart hyphen" rule so `polygon-set-filled` lexes
  as one identifier: `-` joins only when sandwiched between letters).
- `NAPLPS/Telidraw/Parser.cs` — statement dispatch, `ResolveMnemonicOpcode`,
  `ParseMnemonicStatement`, `ParseRawStatement`, `IsNumericLiteralStart`,
  `IsMnemonicStart`.
- `NAPLPS/CommandRegistry.cs` — `OpcodeMnemonics` (ANSI C0 table),
  `TryResolveMnemonic`, `GetOpcodeByKebabName` (8-bit preference + case-insensitive).
- `NAPLPS/Telidraw/Compiler.cs` — `CompileRaw` (bit-7 stripping, verbatim
  operands), `UpdatePenFromRaw`, raw-`DOMAIN` state mirroring.
- `NAPLPS/Telidraw/Decompiler.cs` — the *other* direction; `EmitRaw` shows how the
  UPPERCASE canonical forms are generated (useful to mirror for exact naming).
