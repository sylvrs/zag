const a = 5;
const b = 100;
const c = if (a > b) "gt" else if (a < b) "lt" else "eq";

const d = 1;
if (a > b) {
    var e = 5;
    e *= 5;
    d = e;
} else {
    var e = 10;
    e *= 10;
    d = e;
}
