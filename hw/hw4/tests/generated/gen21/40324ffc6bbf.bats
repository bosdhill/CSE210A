load ../../harness

@test "40324ffc6bbf" {
  check 'y  := ka   ;
y     :=     4     *    z ' '⇒ skip; y := (4*z), {y → 0}
⇒ y := (4*z), {y → 0}
⇒ skip, {y → 0}'
}
