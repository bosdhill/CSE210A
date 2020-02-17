load ../../harness

@test "720efd584521" {
  check 'while false    ∧     2   *    y   = z *-4   do i    :=    y +  -4 ' '⇒ skip, {}'
}
