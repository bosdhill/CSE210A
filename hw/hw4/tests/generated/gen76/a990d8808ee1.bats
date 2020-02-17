load ../../harness

@test "a990d8808ee1" {
  check 'if (true∧  false)   then 
skip    else   x   := l  *-2  ' '⇒ x := (l*-2), {}
⇒ skip, {x → 0}'
}
