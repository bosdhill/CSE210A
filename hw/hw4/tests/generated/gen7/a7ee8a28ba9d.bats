load ../../harness

@test "a7ee8a28ba9d" {
  check 'if (3    < z +   4   ∨  true)      then 
 
 y   :=     y   *3   else  

skip' '⇒ y := (y*3), {}
⇒ skip, {y → 0}'
}
