load ../../harness

@test "7f13172ef1df" {
  check 'y:=     -4- 3    ;  Og    := 1  ' '⇒ skip; Og := 1, {y → -7}
⇒ Og := 1, {y → -7}
⇒ skip, {Og → 1, y → -7}'
}
