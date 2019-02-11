export function objectCompareByValue(obj1, obj2) {
    return JSON.stringify(obj1) == JSON.stringify(obj2);
}

export function mapObject(obj, f) {
    var map = {};
    Object.entries(obj).forEach(x => map[x[0]] = f(x[1]));
    return map;
}

export function swapObjectKeyValue(obj) {
    var swapped = {};

    Object.entries(obj).forEach(x => {
        swapped[x[1]] = x[0];
    });

    return swapped;
}