load ../../harness

@test "fd820273bed1" {
  check 'if (false  ∧  true)      then skip      else     IM   :=     0     *  z  ' '⇒ IM := (0*z), {}
⇒ skip, {IM → 0}'
}
