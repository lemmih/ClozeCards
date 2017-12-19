export const SET_ACTIVE_HIGHLIGHT = "SET_ACTIVE_HIGHLIGHT";
export const FETCH_HIGHLIGHT = "FETCH_HIGHLIGHT";
export const RECEIVE_HIGHLIGHT = "RECEIVE_HIGHLIGHT";
export const ADD_RECENT_WORD = "ADD_RECENT_WORD";
export const CLEAR_HIGHLIGHT = "CLEAR_HIGHLIGHT";

export function setActiveHighlight(word) {
  return {
    type: SET_ACTIVE_HIGHLIGHT,
    payload: word
  };
}
export function fetchHighlight(deckId) {
  return {
    type: FETCH_HIGHLIGHT,
    payload: deckId
  };
}
export function addRecentWord(word) {
  return {
    type: ADD_RECENT_WORD,
    payload: word
  };
}
export function clearHighlight() {
  return {
    type: CLEAR_HIGHLIGHT,
    payload: {}
  };
}
