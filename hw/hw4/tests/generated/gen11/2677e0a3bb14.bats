load ../../harness

@test "2677e0a3bb14" {
  check 'BG   :=     3  + o   ; 
vF:=   -4+ -1' '⇒ skip; vF := (-4+-1), {BG → 3}
⇒ vF := (-4+-1), {BG → 3}
⇒ skip, {BG → 3, vF → -5}'
}
