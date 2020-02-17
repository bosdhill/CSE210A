load ../../harness

@test "8561df441245" {
  check 'y :=   y ;y     :=    z   -  Jx ' '⇒ skip; y := (z-Jx), {y → 0}
⇒ y := (z-Jx), {y → 0}
⇒ skip, {y → 0}'
}
