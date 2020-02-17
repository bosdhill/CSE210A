load ../../harness

@test "ef115b630c9f" {
  check 'z     := I -    z ' '⇒ skip, {z → 0}'
}
