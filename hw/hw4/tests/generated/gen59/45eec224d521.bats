load ../../harness

@test "45eec224d521" {
  check 'x:=z  +-4;
z  :=h5 +    z    ' '⇒ skip; z := (h5+z), {x → -4}
⇒ z := (h5+z), {x → -4}
⇒ skip, {x → -4, z → 0}'
}
