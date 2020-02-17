load ../../harness

@test "ff75d2f2bb53" {
  check 'y  := 4   +  -3;   D    :=    C*    x ' '⇒ skip; D := (C*x), {y → 1}
⇒ D := (C*x), {y → 1}
⇒ skip, {D → 0, y → 1}'
}
