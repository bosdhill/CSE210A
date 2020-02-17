load ../../harness

@test "a47153bc5c3a" {
  check 'y   :=   K+ -1  ;l    := y   +    -1   ' '⇒ skip; l := (y+-1), {y → -1}
⇒ l := (y+-1), {y → -1}
⇒ skip, {l → -2, y → -1}'
}
