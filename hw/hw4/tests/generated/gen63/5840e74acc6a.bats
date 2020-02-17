load ../../harness

@test "5840e74acc6a" {
  check 'if (¬true)   then skip else   
 x    :=     1 *    y  ' '⇒ x := (1*y), {}
⇒ skip, {x → 0}'
}
