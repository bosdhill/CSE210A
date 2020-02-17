load ../../harness

@test "4d63070d7ee3" {
  check 'y  := -2 +   1  ;
x  :=   2 ' '⇒ skip; x := 2, {y → -1}
⇒ x := 2, {y → -1}
⇒ skip, {x → 2, y → -1}'
}
