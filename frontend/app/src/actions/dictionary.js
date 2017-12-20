export const SHOW_DICTIONARY = "SHOW_DICTIONARY";
export const HIDE_DICTIONARY = "HIDE_DICTIONARY";
export const PIN_DICTIONARY = "PIN_DICTIONARY";
export const UNPIN_DICTIONARY = "UNPIN_DICTIONARY";
export const SET_DICTIONARY = "SET_DICTIONARY";
export const DICTIONARY_LOOKUP = "DICTIONARY_LOOKUP";
export const RECEIVE_DICTIONARY_RESULTS = "RECEIVE_DICTIONARY_RESULTS";

export function showDictionary(entry) {
  return {
    type: SHOW_DICTIONARY,
    payload: entry
  };
}
export function hideDictionary() {
  return {
    type: HIDE_DICTIONARY
  };
}
export function pinDictionary(entry) {
  return {
    type: PIN_DICTIONARY,
    payload: entry
  };
}
export function unpinDictionary() {
  return {
    type: UNPIN_DICTIONARY
  };
}
export function setDictionary(entry) {
  return {
    type: UNPIN_DICTIONARY
  };
}
export function dictionaryLookup(words) {
  return {
    type: DICTIONARY_LOOKUP,
    payload: words
  };
}
