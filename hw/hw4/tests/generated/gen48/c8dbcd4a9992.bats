load ../../harness

@test "c8dbcd4a9992" {
  check 'if (false   ∨false)    then     
x:=   x    +x    else  skip' '⇒ skip, {}'
}
