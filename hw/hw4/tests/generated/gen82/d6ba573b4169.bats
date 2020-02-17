load ../../harness

@test "d6ba573b4169" {
  check 'if (false  ∧     true) then  zG :=  z  -    y      else skip    ' '⇒ skip, {}'
}
