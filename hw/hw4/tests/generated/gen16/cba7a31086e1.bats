load ../../harness

@test "cba7a31086e1" {
  check 'o :=  x  + y  ;
y     :=    z   --1     ' '⇒ skip; y := (z--1), {o → 0}
⇒ y := (z--1), {o → 0}
⇒ skip, {o → 0, y → 1}'
}
