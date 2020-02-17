load ../../harness

@test "eca4f250b203" {
  check 'G    :=     x+  z   ; 

y     :=1     -    x ' '⇒ skip; y := (1-x), {G → 0}
⇒ y := (1-x), {G → 0}
⇒ skip, {G → 0, y → 1}'
}
