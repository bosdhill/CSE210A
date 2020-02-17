load ../../harness

@test "2bb382f04195" {
  check 'z   :=     1 +   1   ;
i     :=    v     -  2 ' '⇒ skip; i := (v-2), {z → 2}
⇒ i := (v-2), {z → 2}
⇒ skip, {i → -2, z → 2}'
}
