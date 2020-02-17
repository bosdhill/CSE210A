load ../../harness

@test "b785a3030e7d" {
  check 'while (¬true) do 
x  := -3     *-1' '⇒ skip, {}'
}
