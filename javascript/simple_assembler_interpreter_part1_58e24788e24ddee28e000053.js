function simple_assembler(program) {
  /* return a dictionary with the registers */
  const registers = {};
  let pc = 0;
  for (;;) {
    if (pc < 0 || pc >= program.length) break;
    const cmd = program[pc].split(' ');
    let r, v, isReg;
    switch (cmd[0]) {
      case 'mov':
        r = cmd[1];
        v = cmd[2];
        isReg = /^[a-zA-Z]+$/gi.test(v);
        registers[r] = isReg ? registers[v] : parseInt(v);
        pc++;
        break;
      case 'inc':
        r = cmd[1];
        registers[r]++;
        pc++;
        break;
      case 'dec':
        r = cmd[1];
        registers[r]--;
        pc++;
        break;
      case 'jnz':
        r = cmd[1];
        const offset = parseInt(cmd[2]);
        isReg = /^[a-zA-Z]+$/gi.test(r);
        const val = isReg ? registers[r] : r;
        pc += val !== 0 ? offset : 1;
        break;
      default: throw new Error('unkonwn command: ' + cmd);
    }
  }
  return registers;
}
