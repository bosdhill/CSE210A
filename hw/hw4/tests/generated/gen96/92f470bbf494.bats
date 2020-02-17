load ../../harness

@test "92f470bbf494" {
  check 'while false∧ true     do z     :=   -42161479411963741752208892321403295428  +     x ' '⇒ skip, {}'
}
