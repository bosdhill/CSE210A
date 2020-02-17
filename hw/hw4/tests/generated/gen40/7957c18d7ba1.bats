load ../../harness

@test "7957c18d7ba1" {
  check 'while (¬(y   - -3 <x    *  XI))  do    
 y     :=     y -4  ' '⇒ y := (y-4); while ¬((y--3)<(x*XI)) do { y := (y-4) }, {}
⇒ skip; while ¬((y--3)<(x*XI)) do { y := (y-4) }, {y → -4}
⇒ while ¬((y--3)<(x*XI)) do { y := (y-4) }, {y → -4}
⇒ skip, {y → -4}'
}
