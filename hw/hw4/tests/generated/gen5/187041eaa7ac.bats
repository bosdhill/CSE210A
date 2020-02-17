load ../../harness

@test "187041eaa7ac" {
  check 'z:=     y     *     1    ;   skip ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
