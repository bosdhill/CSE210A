load ../../harness

@test "bea99a3065e9" {
  check 'y :=1   + 2    ;z:=     -4     * z     ' '⇒ skip; z := (-4*z), {y → 3}
⇒ z := (-4*z), {y → 3}
⇒ skip, {y → 3, z → 0}'
}
