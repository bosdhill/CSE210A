load ../../harness

@test "41f768d0fe3d" {
  check 'if (false  ∧     0 * -4 =     -4 *  4)    then y:= 1  * 1     else 

 x :=     v     *   -2  *x  ' '⇒ x := ((v*-2)*x), {}
⇒ skip, {x → 0}'
}
