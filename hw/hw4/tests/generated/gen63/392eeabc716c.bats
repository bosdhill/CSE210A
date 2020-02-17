load ../../harness

@test "392eeabc716c" {
  check 'skip ; x  :=-3 --2    ' '⇒ x := (-3--2), {}
⇒ skip, {x → -1}'
}
