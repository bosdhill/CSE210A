load ../../harness

@test "de9700296aec" {
  check 'if (3     *   1   =   -4   * R9) then z   := z     * z   else   z :=   2   +     1 ' '⇒ z := (2+1), {}
⇒ skip, {z → 3}'
}
