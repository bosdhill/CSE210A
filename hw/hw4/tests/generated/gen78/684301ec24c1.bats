load ../../harness

@test "684301ec24c1" {
  check 'if (1+x   <-2 -    3  ∧     false)     then  
 
skip    else 
 skip' '⇒ skip, {}'
}
