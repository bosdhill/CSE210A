load ../../harness

@test "394e2520ab07" {
  check 'while (¬(-3     +   z     =    -2 *z))  do 

z    :=  -1 -  -2' '⇒ z := (-1--2); while ¬((-3+z)=(-2*z)) do { z := (-1--2) }, {}
⇒ skip; while ¬((-3+z)=(-2*z)) do { z := (-1--2) }, {z → 1}
⇒ while ¬((-3+z)=(-2*z)) do { z := (-1--2) }, {z → 1}
⇒ skip, {z → 1}'
}
