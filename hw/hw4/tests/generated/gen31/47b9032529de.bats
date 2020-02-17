load ../../harness

@test "47b9032529de" {
  check 'while -1  * 1     <     y   -    z      do   

 z  :=3   -     -1' '⇒ z := (3--1); while ((-1*1)<(y-z)) do { z := (3--1) }, {}
⇒ skip; while ((-1*1)<(y-z)) do { z := (3--1) }, {z → 4}
⇒ while ((-1*1)<(y-z)) do { z := (3--1) }, {z → 4}
⇒ skip, {z → 4}'
}
