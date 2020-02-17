load ../../harness

@test "21aa7da5797a" {
  check 'skip     ;  y     :=    x  +     z     ' '⇒ y := (x+z), {}
⇒ skip, {y → 0}'
}
