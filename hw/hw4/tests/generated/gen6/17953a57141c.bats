load ../../harness

@test "17953a57141c" {
  check 'x   := 2+ x;skip     ' '⇒ skip; skip, {x → 2}
⇒ skip, {x → 2}'
}
