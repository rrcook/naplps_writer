// house.td — Simple house drawing using basic shapes
#coord fractions

// Ground
with color 2 {
  move 0.0 0.05
  rect 1.0 0.05
}

// House body
with color 4 {
  move 0.25 0.1
  rect 0.5 0.35
}

// Door
with color 6 {
  move 0.42 0.1
  rect 0.12 0.2
}

// Roof (triangle via polygon)
with color 1 {
  move 0.2 0.45
  polygon 0.5 0.65  0.8 0.45
}

// Window
with color 3 {
  move 0.55 0.3
  rect 0.1 0.1
}

// Sun
with color 5 {
  move 0.85 0.6
  arc 0.88 0.65  0.85 0.6
}
