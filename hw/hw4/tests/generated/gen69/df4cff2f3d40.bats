load ../../harness

@test "df4cff2f3d40" {
  check 'if (¬(KL  +   -4    <     z  -    3))     then 
 skip   else  
z   := -4   -   z  ' '⇒ z := (-4-z), {}
⇒ skip, {z → -4}'
}
