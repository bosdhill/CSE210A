load ../../harness

@test "b8fd88f635ac" {
  check 'while zk    * x <  -3   +  x    ∧true   do z :=    -4 -y     ' '⇒ skip, {}'
}
