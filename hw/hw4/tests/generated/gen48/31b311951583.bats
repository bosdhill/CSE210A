load ../../harness

@test "31b311951583" {
  check 'while (¬(-2-     z   < x     - kw))      do skip' '⇒ skip, {}'
}
