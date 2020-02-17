load ../../harness

@test "cba7f910324c" {
  check 'if (false     ∧     true)   then  y :=  y+z     else 
   skip    ' '⇒ skip, {}'
}
