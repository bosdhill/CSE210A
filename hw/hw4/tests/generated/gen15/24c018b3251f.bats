load ../../harness

@test "24c018b3251f" {
  check 'y  :=   4    ; pR   :=    1 --4  ' '⇒ skip; pR := (1--4), {y → 4}
⇒ pR := (1--4), {y → 4}
⇒ skip, {pR → 5, y → 4}'
}
