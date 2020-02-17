load ../../harness

@test "ccdce53b95b4" {
  check 'x     :=   x   * 27987852870784274667675331796504230718 ;M:=     y    +   z    ' '⇒ skip; M := (y+z), {x → 0}
⇒ M := (y+z), {x → 0}
⇒ skip, {M → 0, x → 0}'
}
