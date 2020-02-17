load ../../harness

@test "21c4409e8554" {
  check 'while (¬true) do 
 
 x     :=   y   +-4 ' '⇒ skip, {}'
}
