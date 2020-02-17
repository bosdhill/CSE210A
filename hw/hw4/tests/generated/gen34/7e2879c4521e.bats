load ../../harness

@test "7e2879c4521e" {
  check 'y:=    E-   x;  tC :=    2   ' '⇒ skip; tC := 2, {y → 0}
⇒ tC := 2, {y → 0}
⇒ skip, {tC → 2, y → 0}'
}
