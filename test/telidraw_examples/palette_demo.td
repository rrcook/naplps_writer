// palette_demo.td — Show all 16 default palette colors as filled rectangles
#coord fractions

let w = 1/8
let h = 1/8

for i in 0..7 {
  color i
  move (i * w) 0.5
  rect w h
}

for i in 8..15 {
  color i
  move ((i - 8) * w) 0.35
  rect w h
}

// Label
with color 7 {
  move 0.25 0.25
  text "NAPLPS Default Palette"
}
