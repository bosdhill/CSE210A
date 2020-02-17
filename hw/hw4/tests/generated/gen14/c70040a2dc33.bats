load ../../harness

@test "c70040a2dc33" {
  check 'if (¬(3     - 3   =     -4   - 2)) then K:=x    +     C      else   
skip' '⇒ K := (x+C), {}
⇒ skip, {K → 0}'
}
