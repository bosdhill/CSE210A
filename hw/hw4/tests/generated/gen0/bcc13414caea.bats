load ../../harness

@test "bcc13414caea" {
  check 'y:=     -1     +    Y ;   skip' '⇒ skip; skip, {y → -1}
⇒ skip, {y → -1}'
}
