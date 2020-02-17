load ../../harness

@test "641db596ff1a" {
  check 'z:= x  -   -2; y:=    y  *   4   ' '⇒ skip; y := (y*4), {z → 2}
⇒ y := (y*4), {z → 2}
⇒ skip, {y → 0, z → 2}'
}
