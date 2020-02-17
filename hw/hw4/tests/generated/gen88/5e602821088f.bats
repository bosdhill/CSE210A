load ../../harness

@test "5e602821088f" {
  check 'if (true   ∨   -1*   x  < 3*y)      then x     :=   3  +  -3     else 
 skip     ' '⇒ x := (3+-3), {}
⇒ skip, {x → 0}'
}
