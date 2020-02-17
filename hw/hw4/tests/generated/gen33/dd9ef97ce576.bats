load ../../harness

@test "dd9ef97ce576" {
  check 'while z *    -3 =     0  +     z  do 
 
 z     :=    1    -  -4' '⇒ z := (1--4); while ((z*-3)=(0+z)) do { z := (1--4) }, {}
⇒ skip; while ((z*-3)=(0+z)) do { z := (1--4) }, {z → 5}
⇒ while ((z*-3)=(0+z)) do { z := (1--4) }, {z → 5}
⇒ skip, {z → 5}'
}
