load ../../harness

@test "f053766c62b8" {
  check 'if (y*   t=   4     - z   ∧ true)  then    
skip    else   skip  ' '⇒ skip, {}'
}
