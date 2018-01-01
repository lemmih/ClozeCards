import {
  SET_ACTIVE_HIGHLIGHT,
  CLEAR_HIGHLIGHT,
  ADD_RECENT_WORD,
  RECEIVE_HIGHLIGHT
} from "../actions/highlight";
import { Set } from "immutable";

const emptyHighlights = {
  active: null,
  expired: Set(),
  recent: Set(),
  known: Set()
};

export default (highlight = emptyHighlights, action) => {
  switch (action.type) {
    case SET_ACTIVE_HIGHLIGHT:
      return { ...highlight, active: action.payload };
    case CLEAR_HIGHLIGHT:
      return emptyHighlights;
    case RECEIVE_HIGHLIGHT: {
      const { expired, recent, known } = action.payload;
      return {
        ...highlight,
        expired: Set(expired),
        recent: Set(recent),
        known: Set(known)
      };
    }
    case ADD_RECENT_WORD: {
      const { recent, expired, known } = highlight;
      const word = action.payload;
      return {
        ...highlight,
        expired: expired.delete(word),
        known: known.delete(word),
        recent: recent.add(word)
      };
    }
    default:
      return highlight;
  }
};
