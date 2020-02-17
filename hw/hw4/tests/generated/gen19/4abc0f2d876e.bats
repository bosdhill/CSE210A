load ../../harness

@test "4abc0f2d876e" {
  check 'if (true∨ -2    -    x    < 0   +   -4) then 
skip  else skip ' '⇒ skip, {}'
}
