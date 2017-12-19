export const FETCH_CARDS = "FETCH_CARDS";
export const RECEIVE_CARDS = "RECEIVE_CARDS";
export const RECEIVE_RESPONSE = "RECEIVE_RESPONSE";
export const CLEAR_CARDS = "CLEAR_CARDS";

export function fetchCards(deckId, style) {
  return {
    type: FETCH_CARDS,
    payload: {
      deckId,
      style
    }
  };
}
export function receiveCards(cards) {
  return {
    type: RECEIVE_CARDS,
    payload: {
      cards
    }
  };
}
export function receiveResponse(response) {
  const userId = JSON.parse(localStorage.getItem("user")).id;
  return {
    type: RECEIVE_RESPONSE,
    payload: Object.assign(
      {},
      { userId: userId, factor: 3, createdAt: new Date() },
      response
    )
  };
}
export function clearCards() {
  return {
    type: CLEAR_CARDS,
    payload: {}
  };
}
