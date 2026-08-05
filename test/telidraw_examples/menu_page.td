// menu_page.td — Videotex-style menu page (Prodigy/Teletext look)
#coord fractions

reset

// Header bar
with color 4 {
  move 0 0.7
  rect 1.0 0.05
}
with color 7 {
  move 0.05 0.71
  text "MAIN MENU"
}

// Numbered options (faux menu items)
with color 7 {
  move 0.05 0.55
  text "1) WEATHER"
  move 0.05 0.45
  text "2) NEWS"
  move 0.05 0.35
  text "3) STOCKS"
  move 0.05 0.25
  text "4) MAIL"
  move 0.05 0.15
  text "5) GAMES"
}

// Selection prompt
with color 5 {
  move 0.05 0.05
  text "Select 1-5: _"
}

// Footer bar
with color 1 {
  move 0 0
  rect 1.0 0.02
}
