import { HIGHSCORE, HIGHSCORE_DELTA } from "../actions/highscore";
import { Map } from "immutable";

export default function(highscore = Map(), action) {
  switch (action.type) {
    case HIGHSCORE:
      return Map(action.payload);
    case HIGHSCORE_DELTA:
      return highscore.merge(Map(action.payload));
    default:
      return highscore;
  }
}
