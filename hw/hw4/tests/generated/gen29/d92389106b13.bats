load ../../harness

@test "d92389106b13" {
  check 'y    :=     1+   0     ;

y    :=    y-   2 ' '⇒ skip; y := (y-2), {y → 1}
⇒ y := (y-2), {y → 1}
⇒ skip, {y → -1}'
}
