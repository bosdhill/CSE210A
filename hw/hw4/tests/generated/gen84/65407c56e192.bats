load ../../harness

@test "65407c56e192" {
  check 'skip     ;z    :=    z     ' '⇒ z := z, {}
⇒ skip, {z → 0}'
}
