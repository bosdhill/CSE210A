load ../../harness

@test "b8e931be86e8" {
  check 'while (¬true) do 
   x :=  0   * y' '⇒ skip, {}'
}
