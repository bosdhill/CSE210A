load ../../harness

@test "2070f485f3e4" {
  check 'x    :=  y*2     ; z    :=   K*    -1' '⇒ skip; z := (K*-1), {x → 0}
⇒ z := (K*-1), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
