export function isSortedAndHow(array:number[]): string {
  let direction = array[0] < array[1] ? 'yes, ascending' : 'yes, descending';
  let last = array[0];
  for (let i = 0; i < array.length; i++) {
    const current = array[i];
    if (direction === 'yes, descending' && current > last || direction === 'yes, ascending' && current < last) {
        direction = 'no';
    }
  last = array[i];
  }
  return direction;
}