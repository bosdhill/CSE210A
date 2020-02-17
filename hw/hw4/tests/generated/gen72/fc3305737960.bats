load ../../harness

@test "fc3305737960" {
  check 'Vg  := 1     +     3    ;  
skip' '⇒ skip; skip, {Vg → 4}
⇒ skip, {Vg → 4}'
}
