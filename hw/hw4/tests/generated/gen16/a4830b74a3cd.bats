load ../../harness

@test "a4830b74a3cd" {
  check 'if (y   =     -4    +  -1     ∧    false)     then o    :=   x* z      else 
  skip' '⇒ skip, {}'
}
