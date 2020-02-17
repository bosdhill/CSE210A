load ../../harness

@test "60adfd0c1880" {
  check 'y:=-2    - -4    ;r    :=   x-   -1 ' '⇒ skip; r := (x--1), {y → 2}
⇒ r := (x--1), {y → 2}
⇒ skip, {r → 1, y → 2}'
}
