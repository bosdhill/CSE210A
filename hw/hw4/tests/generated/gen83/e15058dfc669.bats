load ../../harness

@test "e15058dfc669" {
  check 'while (¬true)     do  
 
skip    ' '⇒ skip, {}'
}
