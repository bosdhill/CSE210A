load ../../harness

@test "85a8d3db0455" {
  check 'if (false    ∨   true)     then  skip    else z :=    4     -     -1  ' '⇒ skip, {}'
}
