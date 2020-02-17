load ../../harness

@test "6bec4470c4d7" {
  check 'o   :=     -1   * i   ;zt  :=     z*     4  ' '⇒ skip; zt := (z*4), {o → 0}
⇒ zt := (z*4), {o → 0}
⇒ skip, {o → 0, zt → 0}'
}
