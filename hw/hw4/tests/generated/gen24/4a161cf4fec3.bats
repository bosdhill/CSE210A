load ../../harness

@test "4a161cf4fec3" {
  check 'if (false∧ -2=    2     +     4)     then  skip else 
x   :=     1-   -4 ' '⇒ x := (1--4), {}
⇒ skip, {x → 5}'
}
