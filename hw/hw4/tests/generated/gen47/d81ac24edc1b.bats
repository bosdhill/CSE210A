load ../../harness

@test "d81ac24edc1b" {
  check 'if (1     <  2--2  ∧  false) then 
skip     else 
skip  ' '⇒ skip, {}'
}
