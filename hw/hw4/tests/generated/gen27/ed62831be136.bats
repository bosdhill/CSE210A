load ../../harness

@test "ed62831be136" {
  check 'while true    ∧0+ZV=   x  +    G5  do 
x:=1 - 0' '⇒ x := (1-0); while (true∧((0+ZV)=(x+G5))) do { x := (1-0) }, {}
⇒ skip; while (true∧((0+ZV)=(x+G5))) do { x := (1-0) }, {x → 1}
⇒ while (true∧((0+ZV)=(x+G5))) do { x := (1-0) }, {x → 1}
⇒ skip, {x → 1}'
}
