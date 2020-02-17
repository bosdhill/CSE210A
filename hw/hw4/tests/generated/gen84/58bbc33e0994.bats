load ../../harness

@test "58bbc33e0994" {
  check 'x:=z;  skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
