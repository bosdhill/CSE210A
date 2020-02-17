load ../../harness

@test "1d793885eebc" {
  check 'x   :=     2 +   -2   ;BF  :=   4 -    0 ' '⇒ skip; BF := (4-0), {x → 0}
⇒ BF := (4-0), {x → 0}
⇒ skip, {BF → 4, x → 0}'
}
