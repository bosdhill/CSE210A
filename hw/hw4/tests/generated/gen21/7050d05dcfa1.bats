load ../../harness

@test "7050d05dcfa1" {
  check 'VG:=    x  -   -4     ; z:=  Cj  *     x  ' '⇒ skip; z := (Cj*x), {VG → 4}
⇒ z := (Cj*x), {VG → 4}
⇒ skip, {VG → 4, z → 0}'
}
