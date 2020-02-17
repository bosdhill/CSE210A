load ../../harness

@test "737917b0a1c5" {
  check 'if (¬(y +x    <     0     *  x))      then 
 z  :=y     -0    else 
 y :=2 *     2 ' '⇒ z := (y-0), {}
⇒ skip, {z → 0}'
}
