load ../../harness

@test "4851c74637dc" {
  check 'while -2=2 +     p     do 
skip   ' '⇒ skip, {}'
}
