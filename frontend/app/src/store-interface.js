import * as Dict from "./actions/dictionary";
import store from "./Store";

let timeout = null;

export function showDictionary(entry) {
  if (timeout) clearTimeout(timeout);
  store.dispatch(Dict.showDictionary(entry));
}
export function hideDictionary() {
  if (timeout) clearTimeout(timeout);
  timeout = setTimeout(() => {
    store.dispatch(Dict.hideDictionary());
  }, 300);
}
