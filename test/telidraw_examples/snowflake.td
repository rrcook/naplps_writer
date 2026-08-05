// snowflake.td — 6-fold symmetric Koch-style snowflake, hand-computed coords
//
// Each of six branches is a 5-segment zig-zag that traces out, makes a
// triangular bump, then retraces back past the origin. The branches are
// rotated 0, 60, 120, 180, 240, 300 degrees from the +X axis. Branch length
// is 0.15, bump length is 0.075.

#coord fractions

with color 4 {
  move 0.5 0.4
  line 0.6500 0.4000
  line 0.6875 0.4650
  line 0.7250 0.4000
  line 0.5750 0.4000
  line 0.4250 0.4000
  move 0.5 0.4
  line 0.5750 0.5299
  line 0.5375 0.5949
  line 0.6125 0.5949
  line 0.5375 0.4650
  line 0.4625 0.3350
  move 0.5 0.4
  line 0.4250 0.5299
  line 0.3500 0.5299
  line 0.3875 0.5949
  line 0.4625 0.4650
  line 0.5375 0.3350
  move 0.5 0.4
  line 0.3500 0.4000
  line 0.3125 0.3350
  line 0.2750 0.4000
  line 0.4250 0.4000
  line 0.5750 0.4000
  move 0.5 0.4
  line 0.4250 0.2701
  line 0.4625 0.2051
  line 0.3875 0.2051
  line 0.4625 0.3350
  line 0.5375 0.4650
  move 0.5 0.4
  line 0.5750 0.2701
  line 0.6500 0.2701
  line 0.6125 0.2051
  line 0.5375 0.3350
  line 0.4625 0.4650
}

with color 7 {
  move 0.45 0.7
  text "SNOWFLAKE"
}
