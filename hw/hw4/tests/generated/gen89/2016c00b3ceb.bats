load ../../harness

@test "2016c00b3ceb" {
  check 'if (true  ∧ y     - -1 <   -2  +    x)      then  
 x :=    x-x     else skip' '⇒ skip, {}'
}
