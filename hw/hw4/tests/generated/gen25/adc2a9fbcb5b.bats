load ../../harness

@test "adc2a9fbcb5b" {
  check 'if (-4     +    x<     -2    *0  ∧-1    <   y   +   z) then KV :=     x -     y  else    skip     ' '⇒ KV := (x-y), {}
⇒ skip, {KV → 0}'
}
