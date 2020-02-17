load ../../harness

@test "1856b62bdf22" {
  check 'if (true    ∧   false)   then 
skip    else z:=1-  y ' '⇒ z := (1-y), {}
⇒ skip, {z → 1}'
}
