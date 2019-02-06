export function mapObject(obj, f) {
    var newObj = {};

    Object.keys(obj).forEach(function (key) {
        newObj[key] = f(obj[key]);
    });

    return newObj;
}

export function swapKeyValueObject(obj) {
    return Object.keys(obj).reduce(function (newObj, key) {
        newObj[obj[key]] = key;
        return newObj;
    }, {});
}