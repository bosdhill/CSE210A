load ../../harness

@test "1dd01ffda31a" {
  check 'z    :=  y-  2    ;y:=  z*     Q ' '⇒ skip; y := (z*Q), {z → -2}
⇒ y := (z*Q), {z → -2}
⇒ skip, {y → 0, z → -2}'
}
