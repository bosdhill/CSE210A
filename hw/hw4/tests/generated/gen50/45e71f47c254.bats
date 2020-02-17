load ../../harness

@test "45e71f47c254" {
  check 'z     := 0 *    y  ;

z   :=   2   *   -4  ' '⇒ skip; z := (2*-4), {z → 0}
⇒ z := (2*-4), {z → 0}
⇒ skip, {z → -8}'
}
