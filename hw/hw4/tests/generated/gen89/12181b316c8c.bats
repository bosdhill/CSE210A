load ../../harness

@test "12181b316c8c" {
  check 'if (0 +    1    =x    *     z) then Td:=  y   +  -2 else x :=0 *0  ' '⇒ x := (0*0), {}
⇒ skip, {x → 0}'
}
