load ../../harness

@test "05f17c178ff3" {
  check 'if (¬false)      then   
 y:= y   -     x     else  

skip' '⇒ y := (y-x), {}
⇒ skip, {y → 0}'
}
