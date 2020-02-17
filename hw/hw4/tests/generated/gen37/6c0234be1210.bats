load ../../harness

@test "6c0234be1210" {
  check 'if (true  ∧    0*   -4 =     3  -  x)      then   skip     else 
skip   ' '⇒ skip, {}'
}
