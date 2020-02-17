load ../../harness

@test "13d204c99faf" {
  check 'y   :=z    +   -1    ;  x  :=    y- -4' '⇒ skip; x := (y--4), {y → -1}
⇒ x := (y--4), {y → -1}
⇒ skip, {x → 3, y → -1}'
}
