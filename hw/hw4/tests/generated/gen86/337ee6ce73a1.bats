load ../../harness

@test "337ee6ce73a1" {
  check 'x     :=   3 +   -1     ;z    :=     4    -3 ' '⇒ skip; z := (4-3), {x → 2}
⇒ z := (4-3), {x → 2}
⇒ skip, {x → 2, z → 1}'
}
