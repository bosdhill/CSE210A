load ../../harness

@test "e0b59a990e52" {
  check 'y  :=    ab -y;  x   :=0' '⇒ skip; x := 0, {y → 0}
⇒ x := 0, {y → 0}
⇒ skip, {x → 0, y → 0}'
}
