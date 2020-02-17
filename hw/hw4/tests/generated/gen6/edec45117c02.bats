load ../../harness

@test "edec45117c02" {
  check 'if (-1    *z    <  3   + -2    ∧false)    then z:=     y    *0  else   z:=2     +     3  ' '⇒ z := (2+3), {}
⇒ skip, {z → 5}'
}
