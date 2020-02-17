load ../../harness

@test "af915f87adc4" {
  check 'if (¬(3     *   1<     1 *  -4))     then  k:=   y    *-1     else x := Ob    *     z     ' '⇒ k := (y*-1), {}
⇒ skip, {k → 0}'
}
