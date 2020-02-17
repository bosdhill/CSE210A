load ../../harness

@test "664b1612a2f6" {
  check 'if (¬(-1 -    4   < y- z))    then  skip   else G6  :=     x    *   z' '⇒ G6 := (x*z), {}
⇒ skip, {G6 → 0}'
}
