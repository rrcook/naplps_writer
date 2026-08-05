// hello.td — Telidraw "Hello World"
// Draws colored text centered on screen
#coord fractions

palette cyan = 1
palette white = 7

with color cyan {
  move 0.3 0.4
  text "Hello NAPLPS!"
}

with color white {
  move 0.2 0.3
  rect 0.6 0.02
}
