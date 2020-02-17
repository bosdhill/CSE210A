load ../../harness

@test "52352b588db9" {
  check 'if (¬false)  then  



x := -4    * 1     else skip' '⇒ x := (-4*1), {}
⇒ skip, {x → -4}'
}
