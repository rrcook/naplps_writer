// clock.td — Analog clock face with 12 hour ticks, hand-computed coordinates
//
// Clock center (0.5, 0.4), radius 0.25. Hour ticks are short radial lines
// drawn from the outer rim (r - 0.03 = 0.22) inward to the center at every
// 30° step around the circle. Hour hand points to the 3 o'clock position
// (right, heading 0) at 0.55 * r. Minute hand points to the 12 o'clock
// position (up, heading 90) at 0.85 * r. Positioning preserves the behavior
// of the original turtle source.

#coord fractions

// 12 hour ticks at every 30 degrees
with color 1 {
  move 0.5000 0.4000
  line 0.7200 0.4000
  move 0.5000 0.4000
  line 0.6905 0.5100
  move 0.5000 0.4000
  line 0.6100 0.5905
  move 0.5000 0.4000
  line 0.5000 0.6200
  move 0.5000 0.4000
  line 0.3900 0.5905
  move 0.5000 0.4000
  line 0.3095 0.5100
  move 0.5000 0.4000
  line 0.2800 0.4000
  move 0.5000 0.4000
  line 0.3095 0.2900
  move 0.5000 0.4000
  line 0.3900 0.2095
  move 0.5000 0.4000
  line 0.5000 0.1800
  move 0.5000 0.4000
  line 0.6100 0.2095
  move 0.5000 0.4000
  line 0.6905 0.2900
}

// Hour hand (right, 3 o'clock position — original heading 0)
with color 2 {
  move 0.5000 0.4000
  line 0.6375 0.4000
  line 0.5000 0.4000
}

// Minute hand (up, 12 o'clock)
with color 4 {
  move 0.5000 0.4000
  line 0.5000 0.6125
  line 0.5000 0.4000
}
