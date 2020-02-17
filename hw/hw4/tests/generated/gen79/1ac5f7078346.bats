load ../../harness

@test "1ac5f7078346" {
  check 'z     :=    4;

 

x :=    z  +  z    ' '⇒ skip; x := (z+z), {z → 4}
⇒ x := (z+z), {z → 4}
⇒ skip, {x → 8, z → 4}'
}
