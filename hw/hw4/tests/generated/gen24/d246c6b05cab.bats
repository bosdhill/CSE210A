load ../../harness

@test "d246c6b05cab" {
  check 'if (true ∨  z-3    <y) then skip    else   
x    :=     y *-3  ' '⇒ skip, {}'
}
