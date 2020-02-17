load ../../harness

@test "96c6f04b6444" {
  check 'if (¬(x   -    x    <  3    *    x)) then 
J    := B1 +   z    else skip    ' '⇒ J := (B1+z), {}
⇒ skip, {J → 0}'
}
