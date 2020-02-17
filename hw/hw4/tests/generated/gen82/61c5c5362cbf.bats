load ../../harness

@test "61c5c5362cbf" {
  check 'while (¬(z    -x   <   -3    -  Vk))      do 
 


z  :=    -3     *   3   ' '⇒ z := (-3*3); while ¬((z-x)<(-3-Vk)) do { z := (-3*3) }, {}
⇒ skip; while ¬((z-x)<(-3-Vk)) do { z := (-3*3) }, {z → -9}
⇒ while ¬((z-x)<(-3-Vk)) do { z := (-3*3) }, {z → -9}
⇒ skip, {z → -9}'
}
