load ../../harness

@test "68e06c546e52" {
  check 'if (¬(z   *    1 =   4 -   y))     then 
 x    :=  -1 else 

skip' '⇒ x := -1, {}
⇒ skip, {x → -1}'
}
