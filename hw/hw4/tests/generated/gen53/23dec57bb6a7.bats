load ../../harness

@test "23dec57bb6a7" {
  check 'x :=     z   *     z   ; z   :=x   -   vZ  ' '⇒ skip; z := (x-vZ), {x → 0}
⇒ z := (x-vZ), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
