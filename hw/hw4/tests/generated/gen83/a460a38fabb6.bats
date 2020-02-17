load ../../harness

@test "a460a38fabb6" {
  check 'if (2  +  -4   < -2    -   -2    ∨     -2 + 4< -2 *   1)      then x:= y     * -4   else    

skip    ' '⇒ x := (y*-4), {}
⇒ skip, {x → 0}'
}
