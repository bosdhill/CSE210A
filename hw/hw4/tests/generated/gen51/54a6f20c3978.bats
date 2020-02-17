load ../../harness

@test "54a6f20c3978" {
  check 'z  :=  e   +   -3   ;
x :=  3   *-1' '⇒ skip; x := (3*-1), {z → -3}
⇒ x := (3*-1), {z → -3}
⇒ skip, {x → -3, z → -3}'
}
