load ../../harness

@test "8912073abbcb" {
  check 'z:=    V -     y  ;skip   ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
