load ../../harness

@test "06def3f1a98d" {
  check 'while z    + 1  <   2-  z    ∨    4   +     w  < x+ gI do  
z :=   -2    *     -2     ' '⇒ z := (-2*-2); while (((z+1)<(2-z))∨((4+w)<(x+gI))) do { z := (-2*-2) }, {}
⇒ skip; while (((z+1)<(2-z))∨((4+w)<(x+gI))) do { z := (-2*-2) }, {z → 4}
⇒ while (((z+1)<(2-z))∨((4+w)<(x+gI))) do { z := (-2*-2) }, {z → 4}
⇒ skip, {z → 4}'
}
