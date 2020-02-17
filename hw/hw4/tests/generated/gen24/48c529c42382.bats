load ../../harness

@test "48c529c42382" {
  check 'while H  <  -3  -    y     ∧ true do skip   ' '⇒ skip, {}'
}
