load ../../harness

@test "5f2daeecdbcc" {
  check 'while (¬(1    * x  <y   *y))      do 




x :=    -2    -     z   ' '⇒ x := (-2-z); while ¬((1*x)<(y*y)) do { x := (-2-z) }, {}
⇒ skip; while ¬((1*x)<(y*y)) do { x := (-2-z) }, {x → -2}
⇒ while ¬((1*x)<(y*y)) do { x := (-2-z) }, {x → -2}
⇒ skip, {x → -2}'
}
