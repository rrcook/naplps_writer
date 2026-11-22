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
| `text-raw "..."`       | text blob with controls | raw bytes incl. C0/C1; see Text controls |

## Attributes 🎛️

| Keyword                       | Notes                                              |
|-------------------------------|----------------------------------------------------|
| `color N`                     | SelectColor: palette index (0-15)                  |
| `color FG BG`                 | SelectColor with bg                                |
| `set-color G R B`             | Define palette entry RGB (3-bit each, 0-7)         |
| `texture line highlight fill` | TEXTURE: line pattern (0-3), highlight bool, fill pattern (0-3) |
| `def-texture mask { … }`      | DEF TEXTURE: define mask A–D (mask 1–4); see Definitions |
| `domain sv mv [dim]`          | DOMAIN: single-byte/multi-byte/dimensionality      |
| `wait n`                      | WAIT: n tenths of a second                         |
| `blink toIdx onTenths offTenths [delay]` | BLINK process                       |
| `field`                       | IncrementalField: full screen                      |
| `field x y w h`               | IncrementalField with bounds                       |
| `reset`                       | RESET (selective)                                  |
| `nsr`                         | Non-Selective Reset                                |

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
- SI (`\x0F`) is written only right after an ESC (`\x1B`).
- Everything else is copied as is.

`\xHH` escapes work in any string literal; string literals are byte strings, so a `\x88` is the single
byte `0x88`.

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

A mask outside 1–4 is a compile error (GCU defines nothing for an operand other than A–D).

### Macros

A macro records a drawing once and replays it on demand. Define it with `@macro proc`; the body's
drawing commands are re-encoded into a DEF MACRO object (`80 <code> <body> A5 85`). The code is
auto-assigned from `0x20` (96 slots, `0x20`–`0x7F`) or given explicitly before the block. Invoke it
with `@name`.

```td
@macro proc star() {
  move 0 0
  line-rel 1/16 1/16
  line-rel -1/16 1/16
}

move 0.2 0.2
@star            // replay the macro at the pen
move 0.6 0.2
@star            // ...and again
```

- `@macro proc name(params) [code] { … }` — definition. Params are bound for the body's geometry; an
  explicit `code` (32–127) overrides the auto-assigned one. A body that draws nothing makes no object.
  DEFP/DEFT variants are saved as a plain DEF MACRO (`0x80`), as in GCU.
- `@name args` — invocation at the current pen: a position command (`A4` absolute for the first
  positioned object, `A5` pen-relative afterwards), the invocation escape `ESC / z ESC o`, the code
  byte, and the code-set restore `ESC ) W ESC ~`. Arguments are accepted but do not change the replayed
  body (GCU plays the recorded definition). An undefined macro is a compile error.

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
