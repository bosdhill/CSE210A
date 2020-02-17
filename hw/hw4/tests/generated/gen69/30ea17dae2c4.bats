load ../../harness

@test "30ea17dae2c4" {
  check 'z :=1    - -1     ;  bF   :=    x  -     z    ' '⇒ skip; bF := (x-z), {z → 2}
⇒ bF := (x-z), {z → 2}
⇒ skip, {bF → -2, z → 2}'
}
