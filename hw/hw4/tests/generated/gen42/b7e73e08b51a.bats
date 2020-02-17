load ../../harness

@test "b7e73e08b51a" {
  check 'Pk:=z+ x   ;x   :=  3   *   1     ' '⇒ skip; x := (3*1), {Pk → 0}
⇒ x := (3*1), {Pk → 0}
⇒ skip, {Pk → 0, x → 3}'
}
