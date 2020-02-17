load ../../harness

@test "0f545dc09ae0" {
  check 'if (y*     y   <  z  *     x   ∨ 4   *-1 <  z  - -4)  then  Nc     :=    z     - -4     else   y :=     x  -x   ' '⇒ Nc := (z--4), {}
⇒ skip, {Nc → 4}'
}
