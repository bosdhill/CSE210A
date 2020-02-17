load ../../harness

@test "8a9c73719e55" {
  check 'while x *  2<     z  -2   do z     :=  -4   * 0 ' '⇒ skip, {}'
}
