load ../../harness

@test "993644ae5703" {
  check 'y:=   z    *3;  skip ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
