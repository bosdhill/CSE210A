load ../../harness

@test "f172a370f966" {
  check 'y :=   -3    * -4    ;    z  :=    -2' '⇒ skip; z := -2, {y → 12}
⇒ z := -2, {y → 12}
⇒ skip, {y → 12, z → -2}'
}
