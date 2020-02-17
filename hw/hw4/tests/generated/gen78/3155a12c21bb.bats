load ../../harness

@test "3155a12c21bb" {
  check 'if (y  -    2    <y) then FK    :=x   * 3  else 
skip  ' '⇒ FK := (x*3), {}
⇒ skip, {FK → 0}'
}
