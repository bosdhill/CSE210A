load ../../harness

@test "172fd3502bba" {
  check 'z    :=-3 *  y    ; x    :=  -1   *  y' '⇒ skip; x := (-1*y), {z → 0}
⇒ x := (-1*y), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
