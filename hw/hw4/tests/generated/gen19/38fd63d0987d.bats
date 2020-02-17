load ../../harness

@test "38fd63d0987d" {
  check 'z   :=   x   *  z ;x    :=   O0 +  -3 ' '⇒ skip; x := (O0+-3), {z → 0}
⇒ x := (O0+-3), {z → 0}
⇒ skip, {x → -3, z → 0}'
}
