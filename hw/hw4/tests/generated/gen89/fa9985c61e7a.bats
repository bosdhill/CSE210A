load ../../harness

@test "fa9985c61e7a" {
  check 'x :=    y   -   -2  ' '⇒ skip, {x → 2}'
}
