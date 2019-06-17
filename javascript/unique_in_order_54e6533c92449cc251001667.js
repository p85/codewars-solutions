var uniqueInOrder=function(iterable){
  //your code here - remember iterable can be a string or an array
  let result = iterable[0] ? [iterable[0]] : [];
  let last = iterable[0];
  for (let i = 1; i < iterable.length; i++) {
    if (iterable[i] !== last) result.push(iterable[i]);
    last = iterable[i];
  }
  return result;
}