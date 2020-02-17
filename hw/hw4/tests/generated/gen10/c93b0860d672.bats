load ../../harness

@test "c93b0860d672" {
  check 'skip  ;z :=   z     -    -4 ' '⇒ z := (z--4), {}
⇒ skip, {z → 4}'
}
