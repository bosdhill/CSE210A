load ../../harness

@test "e50a8d27f94a" {
  check 'if (true    ∧  false)     then 
  l   :=-3 -  -4      else x  :=y * 1  ' '⇒ x := (y*1), {}
⇒ skip, {x → 0}'
}
