load ../../harness

@test "8ea6851a0548" {
  check 'D6:=  x;KI    :=g3 * K    ' '⇒ skip; KI := (g3*K), {D6 → 0}
⇒ KI := (g3*K), {D6 → 0}
⇒ skip, {D6 → 0, KI → 0}'
}
