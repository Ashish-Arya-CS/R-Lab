a=0
input=c(0,0,1,1,1,2,2,3,3,4)

if(length(input)>0)
{
  min=input[1];
  a=1
  for(i in input)
  {
    if(i>min)
    {
      min=i;
      a=a+1;
      
    }
  }
}
print(a)