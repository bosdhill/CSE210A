load ../../harness

@test "47f6ff66023c" {
  check 'z:=   2   +    y    ;  

y  :=  z   + -1 ' '⇒ skip; y := (z+-1), {z → 2}
⇒ y := (z+-1), {z → 2}
⇒ skip, {y → 1, z → 2}'
}
