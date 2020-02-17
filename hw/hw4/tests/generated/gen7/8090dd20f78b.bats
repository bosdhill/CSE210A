load ../../harness

@test "8090dd20f78b" {
  check 'if (y    <    1) then 
skip  else  skip   ' '⇒ skip, {}'
}
