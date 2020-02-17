load ../../harness

@test "a852e7a00830" {
  check 'BX   :=    z- 4   ;


z  :=    1    -    -3 ' '⇒ skip; z := (1--3), {BX → -4}
⇒ z := (1--3), {BX → -4}
⇒ skip, {BX → -4, z → 4}'
}
