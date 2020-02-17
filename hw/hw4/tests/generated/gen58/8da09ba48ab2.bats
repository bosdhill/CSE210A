load ../../harness

@test "8da09ba48ab2" {
  check 'x :=  4   +x  ; 
skip     ' '⇒ skip; skip, {x → 4}
⇒ skip, {x → 4}'
}
