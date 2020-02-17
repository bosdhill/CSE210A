load ../../harness

@test "4f6956ba04f2" {
  check 'z  := y + x     ;
 x   :=y  *z' '⇒ skip; x := (y*z), {z → 0}
⇒ x := (y*z), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
