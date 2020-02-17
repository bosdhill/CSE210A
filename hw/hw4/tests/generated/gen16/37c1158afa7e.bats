load ../../harness

@test "37c1158afa7e" {
  check 'skip ;x :=yL     * -4   ' '⇒ x := (yL*-4), {}
⇒ skip, {x → 0}'
}
