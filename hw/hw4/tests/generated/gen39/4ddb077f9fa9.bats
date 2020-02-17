load ../../harness

@test "4ddb077f9fa9" {
  check 'if (y *  1    <-4    +   x) then    

z   :=  0   * 1   else   skip' '⇒ skip, {}'
}
