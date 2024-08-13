const a = 5;
const b = 1;
const c = if (a > b) "gt" else if (a < b) "lt" else "eq";

const d = 1;
if (a > b) { 
    const e = 5.0;
    d /= e;
} else {
    d *= 100;
}
