load ../../harness

@test "8fbf3eb3330a" {
  check 'while (¬(x    * -2 <1*    y))     do 


y:=    4  -   x  ' '⇒ y := (4-x); while ¬((x*-2)<(1*y)) do { y := (4-x) }, {}
⇒ skip; while ¬((x*-2)<(1*y)) do { y := (4-x) }, {y → 4}
⇒ while ¬((x*-2)<(1*y)) do { y := (4-x) }, {y → 4}
⇒ skip, {y → 4}'
}
