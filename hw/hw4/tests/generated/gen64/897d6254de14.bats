load ../../harness

@test "897d6254de14" {
  check 'y   :=    x-  y ; z   :=2 --3    ' '⇒ skip; z := (2--3), {y → 0}
⇒ z := (2--3), {y → 0}
⇒ skip, {y → 0, z → 5}'
}
