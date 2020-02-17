load ../../harness

@test "dd50586bb276" {
  check 'if (¬(z  <    x    -  x))     then  
skip else x := 3     *     -1  ' '⇒ skip, {}'
}
