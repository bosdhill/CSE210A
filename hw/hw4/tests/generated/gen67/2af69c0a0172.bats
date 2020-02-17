load ../../harness

@test "2af69c0a0172" {
  check 'if (¬true)    then z :=    2 -  2    else 
  skip' '⇒ skip, {}'
}
