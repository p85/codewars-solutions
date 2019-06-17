export function average(scores:number[]):number {
    const result = scores.reduce((cv, a) => cv + a, 0);
    return Math.round(result / scores.length);
}