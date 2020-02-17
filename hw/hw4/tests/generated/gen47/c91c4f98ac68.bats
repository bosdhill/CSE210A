load ../../harness

@test "c91c4f98ac68" {
  check 'z    :=    4 *     z    ;x    :=   y + z' '⇒ skip; x := (y+z), {z → 0}
⇒ x := (y+z), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
