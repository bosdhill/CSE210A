load ../../harness

@test "880d5673a71f" {
  check 'if (-3 *  4 < -3     -z  ∨     false)    then skip else   skip ' '⇒ skip, {}'
}
