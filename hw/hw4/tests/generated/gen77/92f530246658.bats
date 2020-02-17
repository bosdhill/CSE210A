load ../../harness

@test "92f530246658" {
  check 'A2    :=z    +q4   ;   z:=    F  -  J   ' '⇒ skip; z := (F-J), {A2 → 0}
⇒ z := (F-J), {A2 → 0}
⇒ skip, {A2 → 0, z → 0}'
}
