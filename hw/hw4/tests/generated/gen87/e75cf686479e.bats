load ../../harness

@test "e75cf686479e" {
  check 'z   :=   z   +  1    ; P   :=gc   +   m    ' '⇒ skip; P := (gc+m), {z → 1}
⇒ P := (gc+m), {z → 1}
⇒ skip, {P → 0, z → 1}'
}
