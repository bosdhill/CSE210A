load ../../harness

@test "f51fe249891c" {
  check 'while (¬(z  - 3     <M     +  4))     do   x  :=0     ' '⇒ skip, {}'
}
