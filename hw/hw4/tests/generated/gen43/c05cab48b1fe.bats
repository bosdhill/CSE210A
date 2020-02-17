load ../../harness

@test "c05cab48b1fe" {
  check 'while (¬(x +  0 <    -2   *   t))   do   x    :=  2   *     -1     ' '⇒ x := (2*-1); while ¬((x+0)<(-2*t)) do { x := (2*-1) }, {}
⇒ skip; while ¬((x+0)<(-2*t)) do { x := (2*-1) }, {x → -2}
⇒ while ¬((x+0)<(-2*t)) do { x := (2*-1) }, {x → -2}
⇒ skip, {x → -2}'
}
