load ../../harness

@test "f3248091fc7d" {
  check 'z    :=    z -n     ;z    :=     3-  x     ' '⇒ skip; z := (3-x), {z → 0}
⇒ z := (3-x), {z → 0}
⇒ skip, {z → 3}'
}
