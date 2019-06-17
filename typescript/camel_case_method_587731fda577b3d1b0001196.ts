export function camelCase(str: string): string {
  return str.replace(/(^[a-zA-Z]{1}| [a-zA-Z]{1})/g, v => v.toUpperCase()).replace(/ /g, '');
}