function alphabetPosition(text) {
  let result = '';
  for (let i = 0; i < text.length; i++) result += text.toUpperCase().charCodeAt(i) > 64 && text.toUpperCase().charCodeAt(i) < 91 ? (text.toUpperCase().charCodeAt(i) - 64) + ' ' : '';
  return result.slice(0,result.length-1);
}