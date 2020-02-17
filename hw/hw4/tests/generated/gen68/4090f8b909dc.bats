load ../../harness

@test "4090f8b909dc" {
  check 'PF  :=     2     -    z     ;



z    :=    3     *  4    ' '⇒ skip; z := (3*4), {PF → 2}
⇒ z := (3*4), {PF → 2}
⇒ skip, {PF → 2, z → 12}'
}
