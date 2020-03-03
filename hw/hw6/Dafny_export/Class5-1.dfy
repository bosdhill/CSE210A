method double(x:int) returns (y:int)
requires x >= 2;
ensures y >= x*2;
{
	y := x+x;
}

method fib(x:int) returns (y:int)
requires x>=1;
ensures y>=x;
{
	if(x==1 || x==2){
		y:=x;
	}else{
		var a:=fib(x-1);
		var b:=fib(x-2);
		y:=a+b;
	}
}

method mul(x:int, y:int) returns (s:int)
requires x>=0;
ensures s == x*y;
{
	s := 0;
	var c := 0;
	while(c != x)
	invariant s==c*y && c<=x;
	decreases x-c;
	{
		c := c+1;
		s := s+y;
	}
}























