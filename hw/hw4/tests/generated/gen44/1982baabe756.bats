load ../../harness

@test "1982baabe756" {
  check 'if (b -     z  =   -4    + -4 ∧ true)     then Q  := 3  * W  else    skip   ' '⇒ skip, {}'
}
