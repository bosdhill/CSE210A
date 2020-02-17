load ../../harness

@test "c19b3f7ae646" {
  check 'z:=   -4   +     Q ;z  :=-1  *z' '⇒ skip; z := (-1*z), {z → -4}
⇒ z := (-1*z), {z → -4}
⇒ skip, {z → 4}'
}
