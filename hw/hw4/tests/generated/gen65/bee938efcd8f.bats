load ../../harness

@test "bee938efcd8f" {
  check 'y     := un  ;    y    :=    3 +   y ' '⇒ skip; y := (3+y), {y → 0}
⇒ y := (3+y), {y → 0}
⇒ skip, {y → 3}'
}
