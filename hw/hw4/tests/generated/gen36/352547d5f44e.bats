load ../../harness

@test "352547d5f44e" {
  check 'skip   ; L8     :=     z     ' '⇒ L8 := z, {}
⇒ skip, {L8 → 0}'
}
