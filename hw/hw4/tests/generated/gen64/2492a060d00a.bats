load ../../harness

@test "2492a060d00a" {
  check 'if (true  ∨    true)  then   
skip else 
 skip    ' '⇒ skip, {}'
}
