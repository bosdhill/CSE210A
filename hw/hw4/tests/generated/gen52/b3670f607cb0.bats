load ../../harness

@test "b3670f607cb0" {
  check 'z   := -2    *   0   ;
 y    :=   y   - -1 ' '⇒ skip; y := (y--1), {z → 0}
⇒ y := (y--1), {z → 0}
⇒ skip, {y → 1, z → 0}'
}
