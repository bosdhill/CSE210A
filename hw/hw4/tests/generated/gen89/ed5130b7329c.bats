load ../../harness

@test "ed5130b7329c" {
  check 'if (false ∨  -1 *-1     <-3   *    y) then  
  skip   else    
skip' '⇒ skip, {}'
}
