load ../../harness

@test "35b43153643a" {
  check 'while (¬true)      do 
   x    :=x   * 2   ' '⇒ skip, {}'
}
