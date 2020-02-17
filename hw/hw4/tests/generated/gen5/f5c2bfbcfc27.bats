load ../../harness

@test "f5c2bfbcfc27" {
  check 'while (¬true)   do n     :=4 -   z    ' '⇒ skip, {}'
}
