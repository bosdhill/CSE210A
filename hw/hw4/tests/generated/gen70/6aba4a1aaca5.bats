load ../../harness

@test "6aba4a1aaca5" {
  check 'if (¬true)     then skip     else      x :=z  +   x   ' '⇒ x := (z+x), {}
⇒ skip, {x → 0}'
}
