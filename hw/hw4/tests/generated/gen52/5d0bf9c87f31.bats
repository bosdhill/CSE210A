load ../../harness

@test "5d0bf9c87f31" {
  check 'if (k+     Y   <  -2 -     z    ∧false)     then  z:=3    -    b else skip  ' '⇒ skip, {}'
}
