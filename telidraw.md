# Telidraw Language Reference ✍️✨

Telidraw (`.td` files) is the canonical human-editable text format for NAPLPS scenes. Every `.td` file compiles to a `.nap` byte stream; every `.nap` decompiles back to a `.td` that recompiles byte-identical.

## Directives 🧭

```td
#coord fractions          // X in [0,1], Y in [0,0.75]  (default)
#coord pixels             // X,Y in pixel units relative to #resolution
#resolution 640 480       // pixel domain for #coord pixels mode
#bits 7                   // emit 7-bit (0x40-base) opcodes (rare)
#bits 8                   // emit 8-bit (0xC0-base) opcodes (default)
```

## Comments

```td
// single-line
/* multi-line block */
```

## Variables and expressions

```td
let x = 0.5
let w = 1/8                  // fraction literal
let center = (x + 0.1) * 2   // arithmetic
```

## Drawing verbs 🎨

| Keyword                | Emits                  | Args                              |
|------------------------|------------------------|-----------------------------------|
| `move x y`             | PointSetAbsolute       | absolute pen position             |
| `move-rel dx dy`       | PointSetRelative       | delta from pen                    |
| `point x y`            | PointAbsolute          | dot at coords (no pen move)       |
| `point-rel dx dy`      | PointRelative          | dot at pen+delta                  |
| `line x y`             | LineAbsolute           | line to absolute coord            |
| `line-rel dx dy`       | LineRelative           | line to pen+delta                 |
| `line-set x1 y1 x2 y2 ...` | LineSetAbsolute    | N absolute points                 |
| `line-set-rel dx1 dy1 ...` | LineSetRelative    | N relative deltas                 |
| `rect w h`             | RectangleFilled        | filled rect from pen, size w×h    |
| `rect-outline w h`     | RectangleOutlined      | outlined rect                     |
| `rect-set x y w h`     | RectangleSetFilled     | filled rect at absolute origin    |
| `rect-set-outline x y w h` | RectangleSetOutlined |                                 |
| `arc mx my ex ey`      | ArcFilled              | filled arc through mid + end      |
| `arc-outline mx my ex ey` | ArcOutlined         | outlined arc                      |
| `arc-set sx sy mx my ex ey` | ArcSetFilled      | absolute start + mid + end        |
| `arc-set abs sx sy dmx dmy dex dey` | ArcSetFilled | exact: start abs + relative deltas |
| `polygon x1 y1 x2 y2 ...` | PolygonFilled       | filled polygon, vertices relative |
| `polygon-outline x1 y1 ...` | PolygonOutlined   |                                  |
| `polygon-set sx sy v1x v1y ...` | PolygonSetFilled | absolute start, then absolute verts |
| `polygon-set abs sx sy dx1 dy1 ...` | PolygonSetFilled | exact form: start abs + relative tail |
| `text "..."`           | AsciiCharCommand seq   | ASCII string (printable only)     |
| `text set "..."`       | text in another set    | `supplementary`, `mosaic`, `drcs` (or `primary`); see Character sets |
| `text-raw "..."`       | text blob with controls | raw bytes incl. C0/C1; see Text controls |

## Attributes 🎛️

| Keyword                       | Notes                                              |
|-------------------------------|----------------------------------------------------|
| `color N`                     | SelectColor: palette index (0-15)                  |
| `color FG BG`                 | SelectColor with bg                                |
| `set-color G R B`             | Define palette entry RGB (3-bit each, 0-7)         |
| `texture line highlight fill` | TEXTURE: line pattern (0-3), highlight bool, fill pattern (0-3) |
| `texture line highlight fill mw mh` | …and the size of the programmable masks A–D (not negative: GCU drops the mirror) |
| `def-texture mask { … }`      | DEF TEXTURE: define mask A–D (mask 1–4); see Definitions |
| `domain sv mv [dim]`          | DOMAIN: single-byte/multi-byte/dimensionality      |
| `domain sv mv dim pw ph`      | …and the logical pel size (multi-byte 3, 2-D only)  |
| `text-style field value …`    | TEXT: text size and the six text fields; see Text style |
| `wait n`                      | WAIT: n tenths of a second                         |
| `blink toIdx onTenths offTenths [delay]` | BLINK process                       |
| `field`                       | IncrementalField: full screen                      |
| `field x y w h`               | IncrementalField with bounds                       |
| `reset [flag …]`              | RESET (selective); bare `reset` resets nothing; see below |
| `nsr`                         | Non-Selective Reset                                |

Mask and pel sizes are in the current coordinate mode, like `rect` sizes. In a `with texture` or
`with domain` block that gives one, the size in effect before the block is put back at its end.

### RESET flags

`reset` takes any of these words, on the same line:

| Flag        | Bit                  | What GCU's decoder does                                   |
|-------------|----------------------|-----------------------------------------------------------|
| `domain`    | 1st byte, bit 1      | DOMAIN back to its defaults (the pel to 0,0)              |
| `color N`   | 1st byte, bits 3–2   | N = 1–3: colour mode and palette reset (GCU drops these bits when it re-saves) |
| `screen N`  | 1st byte, bits 6–4   | N = 1–7: clear screen / border                            |
| `text`      | 2nd byte, bit 1      | text back to its defaults; cursor and drawing point go home |
| `blink`     | 2nd byte, bit 2      | stored only                                               |
| `fields`    | 2nd byte, bit 3      | stored only                                               |
| `texture`   | 2nd byte, bit 4      | stored only                                               |
| `macros`    | 2nd byte, bit 5      | every macro deleted                                       |
| `drcs`      | 2nd byte, bit 6      | stored only                                               |

```td
reset text macros screen 1     // A0 C8 D1
```

The compiler follows what GCU's decoder does:
- After `reset text`, the pen is at the text home, (0, 190/256). Reverse video and underline are off,
  and the text style is back to its default.
- After `reset macros`, calling a macro defined before it is a compile error. `reset macros` inside a
  macro or texture body is also an error, because GCU restarts the definition it is recording.
- No other flag moves the pen.

## Block structure

```td
proc square(size) {
  repeat 4 {
    forward size
    turn 90
  }
}

with color 5 {
  move 0.1 0.1
  square 0.2
}                  // color restored to whatever it was before the `with`
```

```td
for i in 0..7 {
  color i
  move (i / 8) 0.5
  rect (1/8) 0.1
}
```

## Text controls 🔤

`text` emits printable ASCII only (anything else is filtered out). To put NAPLPS control codes inside
text — CR/LF, reverse video, underline, REPEAT, and so on — use `text-raw` and write the bytes as `\xHH`
escapes (two hex digits; any byte value, including C1 codes `0x80`–`0x9F`):

```td
move 0.1 0.6
text-raw "\x88Warning\x89 line one\x0D\x0Aline two"   // reverse video on/off, then CR LF
text-raw "\x99underlined\x9A"
```

`text-raw` takes the same optional `[width height]` size arguments as `text` and does not move the pen.
Its bytes pass through GCU's text serializer rules:

- NUL (`\x00`) is dropped.
- Reverse video `\x88`/`\x89` and underline `\x99`/`\x9A` are written only when they change state, so a
  repeated "on" (or "off") is dropped. The state carries across `text-raw` commands.
- SI (`\x0F`) is written only right after an ESC (`\x1B`). A file starts as if an ESC had just been seen
  (GCU's starting state), so an SI before any ESC is kept.
- Everything else is copied as is.

`\xHH` escapes work in any string literal; string literals are byte strings, so a `\x88` is the single
byte `0x88`.

After a macro call, `text-raw` starts with an SI, because the call leaves the macro set in GL (see
Macros). Otherwise its bytes are written as given. If they leave another character set in GL, the
next `text` switches back.

## Text style ✒️

`text-style` writes a TEXT command right away with the fields you name. Fields you don't name keep
their current values. Later sized text (`text "..." w h`) uses the same fields. Give all the words on
the `text-style` line.

| Field      | Values                                                  | Default        |
|------------|---------------------------------------------------------|----------------|
| `size W H` | character field size                                    | —              |
| `spacing`  | `1`, `5/4`, `3/2`, `proportional`                       | `proportional` |
| `path`     | `right`, `left`, `up`, `down`                           | `right`        |
| `rotation` | `0`, `90`, `180`, `270`                                 | `0`            |
| `cursor`   | `underscore`, `block`, `crosshair`, `custom`            | `underscore`   |
| `move`     | `together`, `cursor-leads`, `point-leads`, `independent` | `together`     |
| `interrow` | `1`, `5/4`, `3/2`, `2`                                  | `1`            |

```td
text-style size 1/40 5/128 path down cursor block
move 0.1 0.6
text "DOWN"
```

Without `size`, the TEXT command has no size operand, and the text keeps the size it had. `move`
couples the text cursor to the drawing point. With `cursor-leads` or `independent`, a `move` no longer
positions text: text starts at the text cursor. GCU drops text whose start is off the field for its
path. Put the cursor somewhere first with a cursor control in `text-raw`, such as `\x1E` (home).

## Character sets 🔣

`text` takes an optional set word before the string. Each set is written with the switch GCU's own
text writer uses where the set changes:

| Set             | Switch              | Codes                                                         |
|-----------------|---------------------|---------------------------------------------------------------|
| `primary`       | SI                  | printable ASCII (the default; `text "..."` is the same)        |
| `supplementary` | `ESC n`             | `0x20`–`0x3F`, `0x50`–`0x7E`                                   |
| `mosaic`        | `ESC / } ESC o`     | `0x20`–`0x3F`, `0x5F`–`0x7F`                                   |
| `drcs`          | `ESC / { ESC o`     | `0x20`–`0x7F` (the shapes come from the terminal's DRCS set)   |

```td
move 0.1 0.5
text "Score "
text mosaic "\x7f\x7f\x3f"     // switch to mosaics
text " ok"                     // switch back: SI
```

The compiler tracks which set is in GL. The switch is written only when the set changes, and primary
text after another set starts with SI. Two kinds of code are compile errors because GCU does not keep
them:
- Mosaic `0x40`–`0x5E`: GCU prints these as spaces.
- Supplementary `0x40`–`0x4F`: these are non-spacing accents, which GCU writes back as primary-set
  letters.

## Definitions: texture masks and macros 🧩

### Texture masks

`def-texture` defines a texture mask (A–D) that later `texture` fill patterns can use. The mask is
`1`–`4` (encoded as the operand bytes `A`–`D`); the block's drawing commands become the raw mask body
(emitted verbatim, not decoded). Emits `84 <mask> <body> 85` plus the code-set restore `ESC ) W ESC ~`.

```td
def-texture 1 {
  move 1/256 1/256
  rect 2/256 2/256
}
```

A mask outside 1–4 is a compile error (GCU defines nothing for an operand other than A–D). When the
definition ends, the drawing point goes back to (0,0), as GCU does, so relative drawing after a
`def-texture` starts from the origin; `move` first if you want to draw somewhere else.

### Macros

A macro records a drawing once and replays it on demand. Define it with `@macro proc`; the body's
drawing commands are re-encoded into a DEF MACRO object (`80 <code> <body> A5 85`). The code is
auto-assigned from `0x20` (96 slots, `0x20`–`0x7F`) or given explicitly before the block. Invoke it
with `@name`.

```td
@macro proc star() {
  line-rel 1/16 1/16       // relative moves, so the star is drawn wherever the pen is
  line-rel -1/16 1/16
}

move 0.2 0.2
@star            // replay the macro at the pen
move 0.6 0.2
@star            // ...and again
```

- `@macro proc name(params) [code] { … }` — definition. Params are bound for the body's geometry; an
  explicit `code` (32–127) overrides the auto-assigned one. A body that draws nothing makes no object.
  DEFP/DEFT variants are saved as a plain DEF MACRO (`0x80`), as in GCU. Defining a macro does not
  move the pen (the body is recorded, not drawn).
- `@name args` — invocation at the current pen: a position command (`A4` absolute for the first
  positioned object, `A5` pen-relative afterwards), the invocation escape `ESC / z ESC o`, the code
  byte, and the code-set restore `ESC ) W ESC ~`. Arguments are accepted but do not change the replayed
  body (GCU plays the recorded definition). An undefined macro is a compile error. After the call the
  pen is wherever playing the body leaves it, so relative drawing continues from the end of the
  macro's drawing. An absolute `move` inside a body makes the macro draw at that fixed position, not at
  the pen.
- `@name ss3` — the same invocation through SS3 (`ESC / z SS3`, one byte shorter). GCU writes this
  form for a call it read that way. `ss3` must come right after the name.
- **Text after a call starts with SI.** The default invocation's `ESC o` leaves the macro set in GL,
  and GCU reads printable bytes after it as more macro calls, so the compiler puts an SI before the
  next text. The SS3 form leaves GL alone and needs no SI.
- **Calling a macro from another macro's body plays the inner macro twice in GCU.** GCU runs a
  definition while it records it, and it records the bytes the inner call plays as well as the call
  itself, so replaying the outer macro draws the inner one via the call and again from the copy.
- Definitions cannot be nested: `@macro proc` or `def-texture` inside a macro or texture body is a
  compile error, because GCU ends the outer definition at the inner one's opcode.

## Raw fallback

When the decompiler can't find a high-level form (rare 7-bit edge cases, mosaic data, DRCS bitmap bytes), it emits the byte stream verbatim:

```td
raw 0x37 64 90 100 ...   // PolygonSetFilled with N raw operand bytes
```

This is **always** byte-exact. The `// CommandName` comment is for human reading; the compiler ignores it.

## Quick example

```td
// hello.td: minimal NAPLPS scene
#coord fractions

domain 1 3 2
color 7
move 0.1 0.6
text "Hello, NAPLPS!"

with color 1 {
  move 0.1 0.5
  rect 0.8 0.05
}

reset
```

## Byte-fidelity guarantee ✅

For any `.nap` file F:
```
decompile(F) → T   then   compile(T) → F'   ⟹   bytes(F) == bytes(F')
```

Tested across all 375 files in `Examples/` on every commit. See `NAPLPSTests/Telidraw/ExamplesRoundTripTests.cs`.
