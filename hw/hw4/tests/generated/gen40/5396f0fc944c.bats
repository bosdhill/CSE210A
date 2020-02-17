load ../../harness

@test "5396f0fc944c" {
  check 'z:=    -2+   4    ; skip  ' '⇒ skip; skip, {z → 2}
⇒ skip, {z → 2}'
}
