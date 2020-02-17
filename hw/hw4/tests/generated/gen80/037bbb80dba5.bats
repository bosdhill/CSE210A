load ../../harness

@test "037bbb80dba5" {
  check 'if (false ∧   true) then    XT    :=    -2      else Sk :=  x  +    y    ' '⇒ Sk := (x+y), {}
⇒ skip, {Sk → 0}'
}
