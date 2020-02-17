load ../../harness

@test "67215f41b44b" {
  check 'x  :=     4 +-1;
   
SU   :=   tc   +  -2 ' '⇒ skip; SU := (tc+-2), {x → 3}
⇒ SU := (tc+-2), {x → 3}
⇒ skip, {SU → -2, x → 3}'
}
