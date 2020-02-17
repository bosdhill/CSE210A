load ../../harness

@test "65d4230b138d" {
  check 'y  :=     y    -     n;y    :=   x-   -1  ' '⇒ skip; y := (x--1), {y → 0}
⇒ y := (x--1), {y → 0}
⇒ skip, {y → 1}'
}
