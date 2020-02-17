load ../../harness

@test "ef05958f8dfa" {
  check 'skip    ;

y  :=x  +  z     ' '⇒ y := (x+z), {}
⇒ skip, {y → 0}'
}
