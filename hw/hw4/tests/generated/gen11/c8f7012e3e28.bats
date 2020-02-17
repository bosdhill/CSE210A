load ../../harness

@test "c8f7012e3e28" {
  check 'z   :=   x    -    -3   ;
z  := z + i    ' '⇒ skip; z := (z+i), {z → 3}
⇒ z := (z+i), {z → 3}
⇒ skip, {z → 3}'
}
