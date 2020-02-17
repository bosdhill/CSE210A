load ../../harness

@test "599cf43e08e0" {
  check 'x:= x  - -4    ; y     :=    x   +-1 ' '⇒ skip; y := (x+-1), {x → 4}
⇒ y := (x+-1), {x → 4}
⇒ skip, {x → 4, y → 3}'
}
