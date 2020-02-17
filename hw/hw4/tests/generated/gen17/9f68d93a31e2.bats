load ../../harness

@test "9f68d93a31e2" {
  check 'y:=     x   +  x  ;    y := -1   -    Bk' '⇒ skip; y := (-1-Bk), {y → 0}
⇒ y := (-1-Bk), {y → 0}
⇒ skip, {y → -1}'
}
