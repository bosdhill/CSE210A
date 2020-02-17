load ../../harness

@test "e18d07e29a67" {
  check 'z  := 2   -  y ;x := -2  +     z    ' '⇒ skip; x := (-2+z), {z → 2}
⇒ x := (-2+z), {z → 2}
⇒ skip, {x → 0, z → 2}'
}
