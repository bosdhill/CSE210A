load ../../harness

@test "006745832bd1" {
  check 'z    :=     2*z   ;  y:=y  *     -1   ' '⇒ skip; y := (y*-1), {z → 0}
⇒ y := (y*-1), {z → 0}
⇒ skip, {y → 0, z → 0}'
}
