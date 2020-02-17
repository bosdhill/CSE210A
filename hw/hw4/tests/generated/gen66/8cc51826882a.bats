load ../../harness

@test "8cc51826882a" {
  check 'if (x   -y <    -2    +  y   ∨     4     *     y     =x    * y)      then   skip      else    sF   :=2   -1  ' '⇒ skip, {}'
}
