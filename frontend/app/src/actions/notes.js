// @flow
export const FETCH_NOTES = "FETCH_NOTES";
export const RECEIVE_NOTES = "RECEIVE_NOTES";

export type FetchNotesAction = {|
  type: "FETCH_NOTES",
  payload: {|
    userId: string,
    deckId: string
  |}
|};

export type ReceiveNotesAction = {|
  type: "RECEIVE_NOTES",
  payload: {|
    userId: string,
    deckId: string,
    contentId: string
  |}
|};

export function fetchNotes(userId: string, deckId: string): FetchNotesAction {
  return {
    type: FETCH_NOTES,
    payload: { userId, deckId }
  };
}
export function receiveNotes(
  userId: string,
  deckId: string,
  contentId: string
): ReceiveNotesAction {
  return {
    type: RECEIVE_NOTES,
    payload: { userId, deckId, contentId }
  };
}
