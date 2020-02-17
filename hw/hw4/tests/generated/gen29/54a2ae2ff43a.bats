load ../../harness

@test "54a2ae2ff43a" {
  check 'while 1    + -3    <-3 - y  ∨y    *-2   <    z     * 1     do      y:=3     *0  ' '⇒ skip, {}'
}
