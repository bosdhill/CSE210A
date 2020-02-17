load ../../harness

@test "24740e185d3f" {
  check 'y:= y    ;z:= 4     *     z  ' '⇒ skip; z := (4*z), {y → 0}
⇒ z := (4*z), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
