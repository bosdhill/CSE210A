load ../../harness

@test "fba48bf04198" {
  check 'while z  *   z=  z    *   4    do  


z     := 2     ' '⇒ z := 2; while ((z*z)=(z*4)) do { z := 2 }, {}
⇒ skip; while ((z*z)=(z*4)) do { z := 2 }, {z → 2}
⇒ while ((z*z)=(z*4)) do { z := 2 }, {z → 2}
⇒ skip, {z → 2}'
}
