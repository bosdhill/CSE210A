load ../../harness

@test "3086480ef6d6" {
  check 'x :=  -3  +   -3     ; iU  :=  2   + 1 ' '⇒ skip; iU := (2+1), {x → -6}
⇒ iU := (2+1), {x → -6}
⇒ skip, {iU → 3, x → -6}'
}
