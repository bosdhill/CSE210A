load ../../harness

@test "2b71a722bcc9" {
  check 'x     :=-3     - y    ;

 y    :=z+ 2' '⇒ skip; y := (z+2), {x → -3}
⇒ y := (z+2), {x → -3}
⇒ skip, {x → -3, y → 2}'
}
