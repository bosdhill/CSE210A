load ../../harness

@test "6dc08231cb13" {
  check 'while (¬(0- x   <   y    -     0)) do   
 
{x :=     oi     * y   ;  

y    := -1 -     -2}' '⇒ x := (oi*y); y := (-1--2); while ¬((0-x)<(y-0)) do { x := (oi*y); y := (-1--2) }, {}
⇒ skip; y := (-1--2); while ¬((0-x)<(y-0)) do { x := (oi*y); y := (-1--2) }, {x → 0}
⇒ y := (-1--2); while ¬((0-x)<(y-0)) do { x := (oi*y); y := (-1--2) }, {x → 0}
⇒ skip; while ¬((0-x)<(y-0)) do { x := (oi*y); y := (-1--2) }, {x → 0, y → 1}
⇒ while ¬((0-x)<(y-0)) do { x := (oi*y); y := (-1--2) }, {x → 0, y → 1}
⇒ skip, {x → 0, y → 1}'
}
