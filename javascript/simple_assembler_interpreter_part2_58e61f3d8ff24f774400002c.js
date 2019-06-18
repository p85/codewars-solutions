function isRegister(value) {
  return /^[a-zA-Z]+$/gi.test(value);
}

function findAllLabels(program) {
  const labels = {};
  for (let i = 0; i < program.length; i++) {
    if (program[i].charAt(program[i].length - 1) === ':') {
      const label = program[i].slice(0, program[i].length - 1);
      if (!labels[label]) labels[label] = i;
    }
  }
  return labels;
}

function assemblerInterpreter(program) {
  program = program.split('\n').map(pl => pl.trim());
  const registers = {};
  const labels = findAllLabels(program); // label -> index
  const rets = []; // return indexes
  const flags = { notEqual: false, equal: false, greaterEqual: false, greater: false, lessEqual: false, less: false };
  let programOutput = '';
  let pc = 0;
  let terminate = false;
  for (; ;) {
    if (pc < 0 || pc >= program.length || terminate) break;
    const cmd = program[pc].replace(/\;.+/g, '').split(' ').map(c => c.replace(',', '')).filter(f => f);
    if (cmd.length === 0) {
      pc++;
      continue;
    }
    let p1, p2, isRegP1, isRegP2, newPC, val1, val2, offset;
    switch (cmd[0]) {
      case 'mov':
        p1 = cmd[1];
        p2 = cmd[2];
        isRegP1 = isRegister(p2);
        registers[p1] = isRegP1 ? registers[p2] : parseInt(p2);
        pc++;
        break;
      case 'inc':
        p1 = cmd[1];
        registers[p1]++;
        pc++;
        break;
      case 'dec':
        p1 = cmd[1];
        registers[p1]--;
        pc++;
        break;
      case 'add':
        p1 = cmd[1];
        p2 = cmd[2];
        isRegP1 = isRegister(p2);
        registers[p1] += isRegP1 ? registers[p2] : parseInt(p2);
        pc++;
        break;
      case 'sub':
        p1 = cmd[1];
        p2 = cmd[2];
        isRegP1 = isRegister(p2);
        registers[p1] -= isRegP1 ? registers[p2] : parseInt(p2);
        pc++
        break;
      case 'mul':
        p1 = cmd[1];
        p2 = cmd[2];
        isRegP1 = isRegister(p2);
        registers[p1] *= isRegP1 ? registers[p2] : parseInt(p2);
        pc++
        break;
      case 'div':
        p1 = cmd[1];
        p2 = cmd[2];
        isRegP1 = isRegister(p2);
        registers[p1] = registers[p1] / (isRegP1 ? registers[p2] : parseInt(p2)) | 0;
        pc++
        break;
      case 'jmp':
        p1 = cmd[1];
        newPC = labels[p1];
        if (isNaN(newPC)) throw new Error('Label ' + p1 + 'not found');
        pc = newPC;
        break;
      case 'jnz':
        p1 = cmd[1];
        offset = parseInt(cmd[2]);
        isRegP1 = isRegister(p1);
        val1 = isRegP1 ? registers[p1] : p1;
        pc += val1 !== 0 ? offset : 1;
        break;
      // ***
      case 'jne':
        p1 = cmd[1];
        newPC = labels[p1];
        if (isNaN(newPC)) throw new Error('Label ' + p1 + 'not found');
        pc = flags.notEqual ? newPC : pc + 1;
        break;
      case 'je':
        p1 = cmd[1];
        newPC = labels[p1];
        if (isNaN(newPC)) throw new Error('Label ' + p1 + 'not found');
        pc = flags.equal ? newPC : pc + 1;
        break;
      case 'jge':
        p1 = cmd[1];
        newPC = labels[p1];
        if (isNaN(newPC)) throw new Error('Label ' + p1 + 'not found');
        pc = flags.greaterEqual ? newPC : pc + 1;
        break;
      case 'jg':
        p1 = cmd[1];
        newPC = labels[p1];
        if (isNaN(newPC)) throw new Error('Label ' + p1 + 'not found');
        pc = flags.greater ? newPC : pc + 1;
        break;
      case 'jle':
        p1 = cmd[1];
        newPC = labels[p1];
        if (isNaN(newPC)) throw new Error('Label ' + p1 + 'not found');
        pc = flags.lessEqual ? newPC : pc + 1;
        break;
      case 'jl':
        p1 = cmd[1];
        newPC = labels[p1];
        if (isNaN(newPC)) throw new Error('Label ' + p1 + 'not found');
        pc = flags.less ? newPC : pc + 1;
        break;
      case 'call':
        p1 = cmd[1];
        newPC = labels[p1];
        if (isNaN(newPC)) throw new Error('Label ' + p1 + 'not found');
        rets.push(pc);
        pc = newPC;
        break;
      case 'ret':
        if (rets.length === 0) throw new Error('Nothing on stack');
        pc = rets.pop() + 1;
        break;
      case 'msg':
        const a = program[pc].slice(3, program[pc].length).split(', ');
        a[0] = a[0].trimLeft();
        let openBrackets = false;
        for (let i = 0; i < a.length; i++) {
          if (/^\'{1}.+\'{1}$/g.test(a[i])) {
            programOutput += a[i].slice(1, a[i].length - 1);
          } else if (a[i] === '\'') {
            openBrackets = !openBrackets;
            if (!openBrackets) programOutput += ', ';
          } else {
            const reg = a[i].trim().split(' ')[0];
            programOutput += parseInt(registers[reg]);
          }
        }
        pc++;
        break;
      case 'cmp':
        p1 = cmd[1];
        p2 = cmd[2];
        isRegP1 = isRegister(p1);
        isRegP2 = isRegister(p2);
        val1 = isRegP1 ? registers[p1] : parseInt(p1);
        val2 = isRegP2 ? registers[p2] : parseInt(p2);
        flags.notEqual = val1 !== val2;
        flags.equal = val1 === val2;
        flags.greaterEqual = val1 >= val2;
        flags.greater = val1 > val2;
        flags.lessEqual = val1 <= val2;
        flags.less = val1 < val2;
        pc++;
        break;
      case 'end':
        terminate = true;
        break;
      case '':
      case '\n':
        // empty line
        pc++;
        break;
      default:
        if (cmd[0].charAt(0) === ';') {
          // ignore comments
          pc++;
        } else if (cmd[0].charAt(cmd[0].length - 1) === ':') {
          // labels
          pc++;
        } else {
          pc++;
          throw new Error('unkonwn command: ' + cmd);
        }
    }
  }
  return !terminate ? -1 : programOutput.trim();
}