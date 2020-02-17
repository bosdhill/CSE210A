load ../../harness

@test "2a963182bd17" {
  check 'if (true ∧  3     +     -2  <  1+-1)  then 
skip     else 
 
 skip' '⇒ skip, {}'
}
