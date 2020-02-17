load ../../harness

@test "b7be5a80f286" {
  check 'tj :=  2     +    x     ;skip     ' '⇒ skip; skip, {tj → 2}
⇒ skip, {tj → 2}'
}
