load ../../harness

@test "7c0471330031" {
  check 'if (false∨ true)   then 
  z :=   y  *     x else  
skip' '⇒ z := (y*x), {}
⇒ skip, {z → 0}'
}
