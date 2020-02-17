load ../../harness

@test "882bf5055179" {
  check 'if (4 +   0    <   x+ y)      then   
 skip   else skip    ' '⇒ skip, {}'
}
