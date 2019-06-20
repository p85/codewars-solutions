const interpret = code => {
  code = code.split('\n').map(c => c.split(''));
  const currentPosition = [0, 0]; // left, top
  let movingDirection = 1;
  const stack = [];
  let stringMode = false;
  let output = '';
  let terminate = false;
  const ops = {
    '+': () => stack.push(stack.pop() + stack.pop()),
    '-': () => {let a = stack.pop(); let b = stack.pop(); stack.push(b-a)},
    '*': () => stack.push(stack.pop() * stack.pop()),
    '/': () => {let a = stack.pop(); let b = stack.pop(); stack.push(!a ? 0 : b / a) | 0},
    '%': () => stack.push(stack[stack.length - 1] === 0 ? 0 : stack.shift() % stack.pop()),
    '!': () => stack.push(stack.pop() === 0 ? 1 : 0),
    '`': () => stack.push(stack.shift() > stack.pop() ? 1 : 0),
    '>': () => movingDirection = 1,
    '<': () => movingDirection = 3,
    '^': () => movingDirection = 0,
    'v': () => movingDirection = 2,
    '?': () => movingDirection = Math.floor(Math.random() * 4),
    '_': () => stack.pop() === 0 ? ops['>']() : ops['<'](),
    '|': () => stack.pop() === 0 ? ops['v']() : ops['^'](),
    '"': () => stringMode = !stringMode,
    ':': () => stack.push(stack[stack.length - 1] ? stack[stack.length - 1] : 0),
    '\\': () => [stack[stack.length - 1], stack[stack.length - 2]] = [stack[stack.length - 2], stack[stack.length - 1]],
    '$': () => stack.pop(),
    '.': () => output += '' + stack.pop(),
    ',': () => {
      let v = stack.pop();
      output += v < 10 ? String(v) : String.fromCharCode(v);
    },
    '#': () => ops['__adv'](),
    'p': () => code[stack.pop()][stack.pop()] = String.fromCharCode(stack.pop()),
    'g': () => {
      let t = code[stack.pop()][stack.pop()];
      t = /^[0-9]+$/.test(t) ? t : t.charCodeAt(0);
      stack.push(t);
    },
    '@': () => terminate = true,
    ' ': () => null,
    '__adv': () => {
      switch (movingDirection) {
        case 0: currentPosition[1]--; break;
        case 1: currentPosition[0]++; break;
        case 2: currentPosition[1]++; break;
        case 3: currentPosition[0]--; break;
        default: throw new Error('unknown direction: ' + movingDirection)
      }
    }
  };
  while (!terminate) {
    const currentOp = code[currentPosition[1]][currentPosition[0]];
    if ((stringMode && currentOp !== '"') || /^[0-9]+$/.test(currentOp)) {
      stack.push(/^[0-9]+$/.test(currentOp) ? parseInt(currentOp) : currentOp.charCodeAt(0));
      ops['__adv']();
    } else {
      if (!ops[currentOp]) throw new Error('Unknown Operation: ' + currentOp);
      ops[currentOp]();
      ops['__adv']();
    }
  }
  return output;
};
