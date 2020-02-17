load ../../harness

@test "901140d3d166" {
  check 'x  :=z   + x  ;



z:=0+  z  ' '⇒ skip; z := (0+z), {x → 0}
⇒ z := (0+z), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
