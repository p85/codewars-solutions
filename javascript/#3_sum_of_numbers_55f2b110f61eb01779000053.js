function GetSum( a,b )
{
  const min = a < b ? a : b;
  const max = a > b ? a : b;
  let result = 0;
  for (let i = min; i < max; i++) result += i;
  result += max;
  return result;
}