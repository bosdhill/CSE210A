load ../../harness

@test "ce4729107156" {
  check 'if (z  *-2  <    -1   +    x∨     true)  then    
skip else   skip    ' '⇒ skip, {}'
}
