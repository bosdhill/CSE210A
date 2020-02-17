load ../../harness

@test "956f627fbecc" {
  check 'while (¬(z  -     l< 1- 3))    do    
z    :=     -3    - x     ' '⇒ z := (-3-x); while ¬((z-l)<(1-3)) do { z := (-3-x) }, {}
⇒ skip; while ¬((z-l)<(1-3)) do { z := (-3-x) }, {z → -3}
⇒ while ¬((z-l)<(1-3)) do { z := (-3-x) }, {z → -3}
⇒ skip, {z → -3}'
}
