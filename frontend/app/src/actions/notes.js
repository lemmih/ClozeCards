export const FETCH_NOTES = "FETCH_NOTES";
export const RECEIVE_NOTES = "RECEIVE_NOTES";

export function fetchNotes(userId, deckId) {
  return {
    type: FETCH_NOTES,
    payload: { userId, deckId }
  };
}
export function receiveNotes(userId, deckId, contentId) {
  return {
    type: RECEIVE_NOTES,
    payload: { userId, deckId, contentId }
  };
}
