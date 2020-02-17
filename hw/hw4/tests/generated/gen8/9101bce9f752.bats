load ../../harness

@test "9101bce9f752" {
  check 'y:=   4  ;  skip     ' '⇒ skip; skip, {y → 4}
⇒ skip, {y → 4}'
}
