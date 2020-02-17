load ../../harness

@test "754d09fa9587" {
  check 'y:=z    +  -1   ;skip' '⇒ skip; skip, {y → -1}
⇒ skip, {y → -1}'
}
