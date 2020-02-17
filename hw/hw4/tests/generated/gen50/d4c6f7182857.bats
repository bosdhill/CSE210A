load ../../harness

@test "d4c6f7182857" {
  check 'y  :=     1   +  4 ; eV  :=z *    3  ' '⇒ skip; eV := (z*3), {y → 5}
⇒ eV := (z*3), {y → 5}
⇒ skip, {eV → 0, y → 5}'
}
