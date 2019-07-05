function VigenèreCipher(key, abc) {
  
  this.encode = function (str) {
    let result = '';
    let skey = '';
    for (let i = 0; i < key.length * (str.length / key.length); i += key.length) skey += key;
    for (let i = 0; i < str.length; i++) {
      const charindex = [...abc].indexOf(str.charAt(i));
      if (charindex === -1) {
        result += str.charAt(i);
        continue;
      }
      const keyindex = [...abc].indexOf(skey.charAt(i));
      let newindex = charindex + keyindex;
      newindex = newindex >= abc.length ? newindex % abc.length : newindex;
      const newchar = [...abc][newindex];
      result += newchar;
    }
    return result;
  };
  this.decode = function (str) {
    let result = '';
    let skey = '';
    for (let i = 0; i < key.length * (str.length / key.length); i += key.length) skey += key;
    for (let i = 0; i < str.length; i++) {
      const currentChar = str.charAt(i);
      const currentKey = skey.charAt(i);
      const currentCharIndex = [...abc].indexOf(currentChar);
      const currentKeyIndex = [...abc].indexOf(currentKey);
      if (currentCharIndex === -1) {
        result += str.charAt(i);
        continue;
      }
      let r = currentCharIndex - currentKeyIndex;
      r = r < 0 ? [...abc].length - Math.abs(r) : r;
      result += [...abc][r];
    }
    return result;
  };
}