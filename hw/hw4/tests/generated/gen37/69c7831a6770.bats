load ../../harness

@test "69c7831a6770" {
  check 'if (1<   x + z∧  false)      then   x   :=z   + x  else     I  :=  1     +     k     ' '⇒ I := (1+k), {}
⇒ skip, {I → 1}'
}
