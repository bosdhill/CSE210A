load ../../harness

@test "e83fc7dcb5b2" {
  check 'g   :=    f  *x;
z:=1 *   u  ' '⇒ skip; z := (1*u), {g → 0}
⇒ z := (1*u), {g → 0}
⇒ skip, {g → 0, z → 0}'
}
