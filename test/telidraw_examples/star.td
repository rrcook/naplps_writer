// star.td — Five-pointed star, hand-computed vertex coordinates
//
// Each star is 5 line segments whose endpoints are precomputed from the
// original turtle-style (forward size; turn 144) walk. For center (cx, cy)
// and edge length `size` the vertices are:
//   P1 = (cx + size,             cy)
//   P2 = (cx + 0.19098 * size,   cy + 0.58779 * size)
//   P3 = (cx + 0.5     * size,   cy - 0.36327 * size)
//   P4 = (cx + 0.80902 * size,   cy + 0.58779 * size)
//   P5 = (cx,                    cy)
//
// where 0.80902 = cos(36°), 0.58779 = sin(36°), 0.19098 = 1 - cos(36°).

#coord fractions

// Large star centered at (0.35, 0.55) with edge length 0.3
with color 4 {
  move 0.35 0.55
  line 0.65     0.55
  line 0.407295 0.726336
  line 0.5      0.441019
  line 0.592705 0.726336
  line 0.35     0.55
}

// Small star at (0.5, 0.25) with edge length 0.15
with color 2 {
  move 0.5 0.25
  line 0.65     0.25
  line 0.528647 0.338168
  line 0.575    0.195509
  line 0.621353 0.338168
  line 0.5      0.25
}
