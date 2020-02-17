load ../../harness

@test "ea814cbe934e" {
  check 'if (true    ∧ x -    1<    z *     0)    then z    :=0+z else  
 skip' '⇒ z := (0+z), {}
⇒ skip, {z → 0}'
}
