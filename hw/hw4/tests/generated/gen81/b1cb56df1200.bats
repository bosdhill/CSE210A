load ../../harness

@test "b1cb56df1200" {
  check 'if (¬(-3     *  z  <x))    then skip      else   y   := z*   4   ' '⇒ skip, {}'
}
