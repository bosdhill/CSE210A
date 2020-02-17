load ../../harness

@test "6aedc7b9fa16" {
  check 'y :=     x     ;  ZC    :=-1   +    x    ' '⇒ skip; ZC := (-1+x), {y → 0}
⇒ ZC := (-1+x), {y → 0}
⇒ skip, {ZC → -1, y → 0}'
}
