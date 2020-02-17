load ../../harness

@test "5bd516bbfaf1" {
  check 'y   :=  x     -    y ; skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
