load ../../harness

@test "90f830ba1707" {
  check 'z   :=x     ;  z    :=  -3+-4 ' '⇒ skip; z := (-3+-4), {z → 0}
⇒ z := (-3+-4), {z → 0}
⇒ skip, {z → -7}'
}
