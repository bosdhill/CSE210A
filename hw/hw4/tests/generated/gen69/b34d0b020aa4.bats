load ../../harness

@test "b34d0b020aa4" {
  check 'z:=  y  * K    ; skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
