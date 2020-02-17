load ../../harness

@test "e75953a79496" {
  check 'if (yn-y< -4     -3     ∨    true)     then   
skip     else      RW  :=     4 ' '⇒ skip, {}'
}
