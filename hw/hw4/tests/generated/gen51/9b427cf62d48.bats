load ../../harness

@test "9b427cf62d48" {
  check 'if (true  ∨     false)  then skip   else z  := y  + 2    ' '⇒ skip, {}'
}
