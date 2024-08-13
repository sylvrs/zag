const a = 5;
const b = 100;
const c = if (a > b) "gt" else if (a < b) "lt" else "eq";

const d = 1;
if (a > b) { 
    d /= 5.0;
} else {
    d *= 100;
}
