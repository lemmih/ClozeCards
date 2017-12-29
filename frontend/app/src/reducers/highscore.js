import { SET_HIGHSCORE, UPDATE_HIGHSCORE } from "../actions/highscore";
import { Map } from "immutable";

const emptyHighscore = {
  daily: Map(),
  weekly: Map()
};

export default function(highscore = emptyHighscore, action) {
  switch (action.type) {
    case SET_HIGHSCORE: {
      const { daily, weekly } = action.payload;
      return { daily: Map(daily), weekly: Map(weekly) };
    }
    case UPDATE_HIGHSCORE: {
      const { daily, weekly } = action.payload;
      return {
        daily: highscore.daily.merge(Map(daily)),
        weekly: highscore.weekly.merge(Map(weekly))
      };
    }
    default:
      return highscore;
  }
}
