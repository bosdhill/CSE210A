load ../../harness

@test "aec02610a1bd" {
  check 'x:=   4  +     1  ;
skip   ' '⇒ skip; skip, {x → 5}
⇒ skip, {x → 5}'
}
