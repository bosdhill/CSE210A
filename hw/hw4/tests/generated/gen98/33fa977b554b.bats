load ../../harness

@test "33fa977b554b" {
  check 'while y <    x     * z     do  

x:=x   * y  ' '⇒ skip, {}'
}
