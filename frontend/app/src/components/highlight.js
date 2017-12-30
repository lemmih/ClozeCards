import _ from "lodash";
import { PureComponent } from "react";
import {} from "semantic-ui-react";
import { connect } from "react-redux";

import {} from "../actions";

import { is, Set } from "immutable";

// prop types
//   target
//   deck_id
//   highlight: { active, expired, recent, unknown }
class Highlight extends PureComponent {
  wordMap = {};
  updLabels = (prevWords, newWords, label) => {
    if (!is(prevWords, newWords)) {
      const removed = prevWords.subtract(newWords);
      const added = newWords.subtract(prevWords);
      removed.forEach(recent => {
        _.forEach(this.wordMap[recent], node => {
          node.classList.remove(label);
        });
      });

      added.forEach(recent => {
        _.forEach(this.wordMap[recent], node => {
          node.classList.add(label);
        });
      });
    }
  };
  componentDidUpdate = prevProps => {
    const { target, highlight } = this.props;
    const node = target.editor.refs.editor;
    const nodes = node.getElementsByClassName("chinese");
    var changed = false;

    if (_.size(this.wordMap) === 0) {
      changed = true;
      _.forEach(nodes, node => {
        const key = node.innerText;
        if (_.has(this.wordMap, key)) {
          this.wordMap[key].push(node);
        } else {
          this.wordMap[key] = [node];
        }
      });
    }
    if (!Object.is(prevProps.highlight.active, highlight.active)) {
      _.forEach(this.wordMap[prevProps.highlight.active], node => {
        node.classList.remove("active");
      });
    }
    if (highlight.active) {
      const lst = this.wordMap[highlight.active];
      _.forEach(lst, node => {
        node.classList.add("active");
      });
      if (lst.length > 0) {
        const eltTopY = lst[0].offsetTop;
        window.scrollTo(0, eltTopY - 20);
      }
    }

    this.updLabels(
      changed ? Set() : prevProps.highlight.recent,
      highlight.recent,
      "recent"
    );
    this.updLabels(
      changed ? Set() : prevProps.highlight.expired,
      highlight.expired,
      "expired"
    );
    this.updLabels(
      changed ? Set() : prevProps.highlight.known,
      highlight.known,
      "known"
    );

    // _.forEach(this.wordMap["我"], node => {
    //   node.className += " active";
    // });
    // nodes[0].className += " active";
    // nodes[1].className += " recent";
    // nodes[2].className += " expired";
    // nodes[3].className += " unknown";
    // console.log(nodes[0]);
  };
  render = () => {
    return null;
  };
}
function toHighlightProps(store) {
  return { highlight: store.highlight };
}
export default connect(toHighlightProps)(Highlight);
