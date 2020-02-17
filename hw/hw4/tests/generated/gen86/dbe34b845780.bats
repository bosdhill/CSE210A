load ../../harness

@test "dbe34b845780" {
  check 'z    := x   +  z;

P :=     z     +-2     ' '⇒ skip; P := (z+-2), {z → 0}
⇒ P := (z+-2), {z → 0}
⇒ skip, {P → -2, z → 0}'
}
