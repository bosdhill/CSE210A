load ../../harness

@test "6afa43a806ca" {
  check 'z     :=  z  ; z    :=   3  ' '⇒ skip; z := 3, {z → 0}
⇒ z := 3, {z → 0}
⇒ skip, {z → 3}'
}
