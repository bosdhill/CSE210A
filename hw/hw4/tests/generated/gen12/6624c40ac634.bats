load ../../harness

@test "6624c40ac634" {
  check 'z :=  3 --1   ' '⇒ skip, {z → 4}'
}
