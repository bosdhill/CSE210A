load ../../harness

@test "f43beef95607" {
  check 'if (-2-p =     0    *     0  ∧ false) then FD     := -3  *    Q6    else  skip   ' '⇒ skip, {}'
}
