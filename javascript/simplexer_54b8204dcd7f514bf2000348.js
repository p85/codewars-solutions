function Simplexer(buffer) {
  this.currentPos = 0;
  
  this.hasNext = function() {
      return this.currentPos < buffer.length;
  };
  
  this.next = function() {
      if (!this.hasNext()) return;
      let i = 0;
      let char = buffer.charAt(this.currentPos);
      let stringMode = char === '"';
      if (stringMode) {
        this.currentPos++;
        i++;
        char = '"' + buffer.charAt(this.currentPos);
      }
      while(true) {
        if ((stringMode && char.charAt(i) === '"') || !this.hasNext()) break;
        if (!stringMode && (char.charAt(i) === '(' || char.charAt(i) === ')' || (char.charAt(i) === ' ' && /[^\s]/.test(buffer.charAt(this.currentPos+1))))) {
          if (char.length > 1) {
            char = char.slice(0,char.length-1);
          } else {
            this.currentPos++;
          }
          break;
        }
        this.currentPos++;
        i++;
        char += buffer.charAt(this.currentPos);
      }
      if (/\s+/.test(char)) return new Token(char, 'whitespace');
      if (/^(true|false)$/.test(char)) return new Token(char, 'boolean');
      if (/^(\+|\-|\*|\/|\%|\(|\)|\=)$/.test(char)) return new Token(char, 'operator');
      if (/^[0-9]+$/.test(char)) return new Token(char, 'integer');
      if (/^(if|else|for|while|return|func|break)$/.test(char)) return new Token(char, 'keyword');
      if (stringMode) return new Token(char, 'string');
      return new Token(char, 'identifier');
  };
}