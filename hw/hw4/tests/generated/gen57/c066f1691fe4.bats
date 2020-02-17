load ../../harness

@test "c066f1691fe4" {
  check 'z     :=     4- z   ;t   := 2 *-3 ' '⇒ skip; t := (2*-3), {z → 4}
⇒ t := (2*-3), {z → 4}
⇒ skip, {t → -6, z → 4}'
}
