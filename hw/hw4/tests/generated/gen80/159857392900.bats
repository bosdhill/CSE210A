load ../../harness

@test "159857392900" {
  check 'z := Em   +   -3   ;
x  :=     y*    z    ' '⇒ skip; x := (y*z), {z → -3}
⇒ x := (y*z), {z → -3}
⇒ skip, {x → 0, z → -3}'
}
