load ../../harness

@test "73d8ad33e6e3" {
  check 'x    := 3     -  2    ;y:=  x   -  0    ' '⇒ skip; y := (x-0), {x → 1}
⇒ y := (x-0), {x → 1}
⇒ skip, {x → 1, y → 1}'
}
