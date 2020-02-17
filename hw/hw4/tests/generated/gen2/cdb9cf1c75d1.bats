load ../../harness

@test "cdb9cf1c75d1" {
  check 'if (false    ∧    false)   then n  :=   -4  *  z      else     skip ' '⇒ skip, {}'
}
