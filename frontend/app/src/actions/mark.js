export const MARK_WORDS = "MARK_WORDS";
export const FETCH_KNOWN_WORDS = "FETCH_KNOWN_WORDS";
export const RECEIVE_KNOWN_WORDS = "RECEIVE_KNOWN_WORDS";

export function markWords(words, known) {
  return {
    type: MARK_WORDS,
    payload: {
      words,
      known
    }
  };
}
export function fetchKnownWords() {
  return { type: FETCH_KNOWN_WORDS, payload: {} };
}
