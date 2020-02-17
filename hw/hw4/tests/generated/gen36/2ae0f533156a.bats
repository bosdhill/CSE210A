load ../../harness

@test "2ae0f533156a" {
  check 'if (true∨true    ∧    false)   then 
skip else x   :=    2    +     -4   ' '⇒ skip, {}'
}
