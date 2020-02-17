load ../../harness

@test "88e708323bdf" {
  check 'x    := y  *  z   ; K4:=    -3    *     y   ' '⇒ skip; K4 := (-3*y), {x → 0}
⇒ K4 := (-3*y), {x → 0}
⇒ skip, {K4 → 0, x → 0}'
}
