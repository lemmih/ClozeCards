export const RECEIVE_DECK = "RECEIVE_DECK";
export const FETCH_DECK = "FETCH_DECK";
export const UNUSED_SLUG = "UNUSED_SLUG";
export const SET_VISIBILITY = "SET_VISIBILITY";

export function receiveDeck(deck) {
  return {
    type: RECEIVE_DECK,
    payload: {
      ...deck,
      owner: JSON.parse(localStorage.getItem("user")).id
    }
  };
}

export function fetchDeck(slug) {
  return {
    type: FETCH_DECK,
    payload: slug
  };
}

export function setVisibility(deckId, hidden) {
  return {
    type: SET_VISIBILITY,
    payload: { deckId, hidden }
  };
}
