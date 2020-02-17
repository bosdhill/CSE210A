load ../../harness

@test "06a49c5a57a7" {
  check 'WJ:=0 * 2   ; Z :=  lW+-1     ;skip  ' '⇒ skip; Z := (lW+-1); skip, {WJ → 0}
⇒ Z := (lW+-1); skip, {WJ → 0}
⇒ skip; skip, {WJ → 0, Z → -1}
⇒ skip, {WJ → 0, Z → -1}'
}
