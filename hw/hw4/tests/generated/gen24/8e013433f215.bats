load ../../harness

@test "8e013433f215" {
  check 'if (¬(b     *     4     =   -4   * x))   then 
  x    :=    z     else 
  TE   :=  x -   z ' '⇒ TE := (x-z), {}
⇒ skip, {TE → 0}'
}
