import { objectCompareByValue } from './obj_utils';

Map.prototype.findByObjectIndex = function (obj) {
    var first = null;
    this.forEach((v, k) => {
        if (objectCompareByValue(k, obj)) {
            first = v;
        }
    });
    return first;
}

export default Map;