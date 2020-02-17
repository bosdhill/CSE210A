load ../../harness

@test "fcd1775b07a9" {
  check 'if (¬(3*     1 =     x     +-1))      then D    :=  x  - -1 else  z    :=    -2    +-3  ' '⇒ D := (x--1), {}
⇒ skip, {D → 1}'
}
