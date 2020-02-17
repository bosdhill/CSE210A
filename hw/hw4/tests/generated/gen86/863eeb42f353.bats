load ../../harness

@test "863eeb42f353" {
  check 'while true  ∧   1  *x   =     qr *   bB do 



x  :=     4+   y    ' '⇒ x := (4+y); while (true∧((1*x)=(qr*bB))) do { x := (4+y) }, {}
⇒ skip; while (true∧((1*x)=(qr*bB))) do { x := (4+y) }, {x → 4}
⇒ while (true∧((1*x)=(qr*bB))) do { x := (4+y) }, {x → 4}
⇒ skip, {x → 4}'
}
