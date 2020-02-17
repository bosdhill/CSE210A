load ../../harness

@test "c0eadaf9aa8c" {
  check 'if (¬(z   * -2  <     1 +    3))   then  
 skip   else An  :=1   -     2     ' '⇒ An := (1-2), {}
⇒ skip, {An → -1}'
}
