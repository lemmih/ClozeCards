// @flow
import * as Search from "./search";
import { FETCH_CARDS } from "./cards";
import { FETCH_CONTENT } from "./content";
import { FETCH_DECK } from "./decks";
import * as Notes from "./notes";

export type Action =
  | Notes.FetchNotesAction
  | Notes.ReceiveNotesAction
  | Search.FetchSearchResultsAction;

export function isQuery(action: Action) {
  switch (action.type) {
    case Search.FETCH_SEARCH_RESULTS:
    case FETCH_CARDS:
    case FETCH_CONTENT:
    case FETCH_DECK:
    case Notes.FETCH_NOTES:
      return true;
    // case Notes.RECEIVE_NOTES:
    //   return false;
    default:
      // console.log("ERROR! Not defined for action:", action.type);
      return false;
  }
}

export * from "./search";
export * from "./cards";
export * from "./content";
export * from "./decks";
export * from "./notes";
export * from "./highlight";
