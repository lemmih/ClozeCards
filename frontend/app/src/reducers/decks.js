import { RECEIVE_DECK, SET_VISIBILITY } from "../actions/decks";
import { SET_FAVORITE, UNSET_FAVORITE } from "../actions/user";
import { Map } from "immutable";

export default function(state = Map(), action) {
  switch (action.type) {
    case RECEIVE_DECK:
      return state.set(action.payload.id, action.payload);
    case SET_FAVORITE:
      return state.update(action.payload, deck => {
        return { ...deck, nLikes: deck.nLikes + 1 };
      });
    case UNSET_FAVORITE:
      return state.update(action.payload, deck => {
        return { ...deck, nLikes: deck.nLikes - 1 };
      });
    case SET_VISIBILITY:
      return state.update(action.payload.deckId, deck => {
        return { ...deck, hidden: action.payload.hidden };
      });
    default:
      return state;
  }
}
