function findUniq(arr) {
  const a = arr.reduce((pv,cv) => {
    if (!pv) pv = {};
    pv[cv] = pv[cv] == undefined ? 1 : pv[cv] + 1;
     return pv;
  }, {});
  let result;
  Object.entries(a).forEach(key => {
    if (key[1] === 1) result = parseFloat(key[0]);
  });
  return result;
}