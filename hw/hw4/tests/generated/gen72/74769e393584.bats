load ../../harness

@test "74769e393584" {
  check 'if (-2   *     z = z  +-3 ∧  0  =0     *x)    then      if (y = y   * x  ∧    y     *    x    <  Q7    +   -4 -   T)   then  skip    else  x   := V *    -4      else  skip' '⇒ skip, {}'
}
