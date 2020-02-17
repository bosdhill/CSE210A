load ../../harness

@test "30b4929ad938" {
  check 'if (false ∧ x     + 3  <  2    *   2) then x  :=    x    +-4   else skip  ' '⇒ skip, {}'
}
