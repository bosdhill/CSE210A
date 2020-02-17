load ../../harness

@test "75037b8743d6" {
  check 'skip     ; y:=   y   -   -2    ' '⇒ y := (y--2), {}
⇒ skip, {y → 2}'
}
