load ../../harness

@test "c441d22c5ec1" {
  check 'if (false ∧     T     *  y     <-2    *     -4)     then y:= 2   --3     else   

x :=    3     *    y ' '⇒ x := (3*y), {}
⇒ skip, {x → 0}'
}
