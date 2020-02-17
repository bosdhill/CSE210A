load ../../harness

@test "58bdd2345cf7" {
  check 'if (x    +    -1    < 2    *     2     ∧     true)      then   
  x   := y  *   -4   else skip     ' '⇒ x := (y*-4), {}
⇒ skip, {x → 0}'
}
