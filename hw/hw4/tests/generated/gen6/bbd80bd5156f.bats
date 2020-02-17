load ../../harness

@test "bbd80bd5156f" {
  check 'skip  ; x   :=     z   *2  ' '⇒ x := (z*2), {}
⇒ skip, {x → 0}'
}
