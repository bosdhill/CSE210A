load ../../harness

@test "82519247ca25" {
  check 'while z    *   -3 =  x- y     ∧   true   do z   :=-3  + 2' '⇒ z := (-3+2); while (((z*-3)=(x-y))∧true) do { z := (-3+2) }, {}
⇒ skip; while (((z*-3)=(x-y))∧true) do { z := (-3+2) }, {z → -1}
⇒ while (((z*-3)=(x-y))∧true) do { z := (-3+2) }, {z → -1}
⇒ skip, {z → -1}'
}
