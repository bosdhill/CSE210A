load ../../harness

@test "ad3098b54222" {
  check 'if (x  +4   < 4 +     3)    then 
  skip  else z     :=-3 - -2     ' '⇒ skip, {}'
}
