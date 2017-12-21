import _ from "lodash";
import React, { PureComponent } from "react";
import { Icon, Loader } from "semantic-ui-react";
import { connect } from "react-redux";
import { unpinDictionary, dictionaryLookup } from "../actions/dictionary";
import "./dictionary.css";

import backend from "../backend";

/*
pinned::boolean
simplified::string
pinyin :: string
english :: string
definitions::[{pinyin: string, english: string[]}]
origin: {sentence_id, offset} OR entity ID + text id.
*/
export default connect(({ dictionary }) => {
  const { activeWord, cache } = dictionary;
  return {
    activeWord,
    definitions: activeWord && cache.get(activeWord.simplified)
  };
})(
  class extends PureComponent {
    componentDidMount = () => {
      const { activeWord, definitions } = this.props;
      if (!definitions && activeWord)
        backend.relay(dictionaryLookup([activeWord.simplified]));
    };
    componentDidUpdate = () => {
      const { activeWord, definitions } = this.props;
      if (!definitions && activeWord)
        backend.relay(dictionaryLookup([activeWord.simplified]));
    };
    setEnglish = english => () => {
      console.log("English", english);
    };
    unpin = () => {
      this.props.dispatch(unpinDictionary());
    };
    render = () => {
      if (!this.props.activeWord) return null;
      const { simplified, english, pinned } = this.props.activeWord;
      const definitions =
        this.props.activeWord.definitions || this.props.definitions;
      const pinyin =
        this.props.activeWord.pinyin ||
        (definitions && _.join(_.map(definitions, def => def.pinyin), ", "));
      return (
        <div className="dictionary">
          {pinned && (
            <div className="close">
              <Icon
                onClick={this.unpin}
                name="remove circle outline"
                size="large"
              />
            </div>
          )}
          <div className="pinyin">{pinyin}</div>
          <div className="chinese">{simplified}</div>
          {english && <div className="english">{english}</div>}
          <div className="definitions">
            {_.isUndefined(definitions) ? (
              <Loader active />
            ) : (
              <ul>
                {definitions.map((lst, idx) => (
                  <li key={idx + JSON.stringify(lst)}>
                    {lst.english.map(elt => (
                      <span key={elt}>
                        <span onClick={this.setEnglish(elt)}>{elt}</span>
                      </span>
                    ))}
                  </li>
                ))}
              </ul>
            )}
          </div>
        </div>
      );
    };
  }
);
