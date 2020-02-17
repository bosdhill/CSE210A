load ../../harness

@test "517e4234a928" {
  check 'while -1     *     JR     =  z   * -4∧  true     do  


z    :=   1     - x    ' '⇒ z := (1-x); while (((-1*JR)=(z*-4))∧true) do { z := (1-x) }, {}
⇒ skip; while (((-1*JR)=(z*-4))∧true) do { z := (1-x) }, {z → 1}
⇒ while (((-1*JR)=(z*-4))∧true) do { z := (1-x) }, {z → 1}
⇒ skip, {z → 1}'
}
