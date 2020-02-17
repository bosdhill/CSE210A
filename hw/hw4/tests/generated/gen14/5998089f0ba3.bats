load ../../harness

@test "5998089f0ba3" {
  check 'z    := x -  1 ; x    :=   J*-2  ' '⇒ skip; x := (J*-2), {z → -1}
⇒ x := (J*-2), {z → -1}
⇒ skip, {x → 0, z → -1}'
}
