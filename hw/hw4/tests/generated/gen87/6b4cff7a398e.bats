load ../../harness

@test "6b4cff7a398e" {
  check 'if (¬(2  +   4     =  -3))     then    skip     else x  := 3    + T1' '⇒ skip, {}'
}
