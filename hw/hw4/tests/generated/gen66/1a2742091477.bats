load ../../harness

@test "1a2742091477" {
  check 'x:= y +   4    ;
skip  ' '⇒ skip; skip, {x → 4}
⇒ skip, {x → 4}'
}
