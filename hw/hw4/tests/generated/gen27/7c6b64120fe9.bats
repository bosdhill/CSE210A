load ../../harness

@test "7c6b64120fe9" {
  check 'z:=     gV     * -2    ;  
x  :=3     *   3     ' '⇒ skip; x := (3*3), {z → 0}
⇒ x := (3*3), {z → 0}
⇒ skip, {x → 9, z → 0}'
}
