import { FETCH_NOTES, RECEIVE_NOTES } from "../actions/notes";
import { Map } from "immutable";

// notes map (userId, deckId) to contentId
export default (notes = Map(), action) => {
  switch (action.type) {
    case FETCH_NOTES: {
      const { userId, deckId } = action.payload;
      return notes.set(userId + "-" + deckId, "fetching");
    }
    case RECEIVE_NOTES: {
      const { userId, deckId, contentId } = action.payload;
      return notes.set(userId + "-" + deckId, contentId);
    }
    default:
      return notes;
  }
};
