load ../../harness

@test "cee498f6ee1f" {
  check 'if (y -    2<  1)  then   skip    else 
   x     :=  1     ' '⇒ skip, {}'
}
