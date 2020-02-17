load ../../harness

@test "2f8cadeef77b" {
  check 'z     :=    k  -     0 ;x:=     z     -   -1' '⇒ skip; x := (z--1), {z → 0}
⇒ x := (z--1), {z → 0}
⇒ skip, {x → 1, z → 0}'
}
