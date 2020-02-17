load ../../harness

@test "dec36be0bd2e" {
  check 'BK  :=   2     ;  z     :=     4*    3  ' '⇒ skip; z := (4*3), {BK → 2}
⇒ z := (4*3), {BK → 2}
⇒ skip, {BK → 2, z → 12}'
}
