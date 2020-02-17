load ../../harness

@test "4733028a7078" {
  check 'd  := y     -     y;
  y    := 3  --4' '⇒ skip; y := (3--4), {d → 0}
⇒ y := (3--4), {d → 0}
⇒ skip, {d → 0, y → 7}'
}
