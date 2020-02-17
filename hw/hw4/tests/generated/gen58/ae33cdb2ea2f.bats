load ../../harness

@test "ae33cdb2ea2f" {
  check 'while (¬true)   do z     :=x  --4   ' '⇒ skip, {}'
}
