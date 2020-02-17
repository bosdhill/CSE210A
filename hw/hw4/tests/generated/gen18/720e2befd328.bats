load ../../harness

@test "720e2befd328" {
  check 'if (0    +  -3   <    L   *3   ∧   true)      then  
  skip     else 
 skip' '⇒ skip, {}'
}
