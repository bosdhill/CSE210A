load ../../harness

@test "2de37f14ee07" {
  check 'x := 2  *z   ; 
skip    ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
