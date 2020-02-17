load ../../harness

@test "1589720c980b" {
  check 'q:=  2     + 3   ' '⇒ skip, {q → 5}'
}
