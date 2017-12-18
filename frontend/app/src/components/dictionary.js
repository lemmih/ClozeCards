import React, { PureComponent } from "react";
import { Icon } from "semantic-ui-react";
import { connect } from "react-redux";
import { unpinDictionary } from "../actions/dictionary";
import "./dictionary.css";

/*
pinned::boolean
simplified::string
pinyin :: string
english :: string
definitions::[{pinyin: string, english: string[]}]
origin: {sentence_id, offset} OR entity ID + text id.
*/
export default connect(({ dictionary }) => {
  return { dictionary };
})(
  class extends PureComponent {
    setEnglish = english => () => {
      console.log("English", english);
    };
    unpin = () => {
      this.props.dispatch(unpinDictionary());
    };
    render = () => {
      if (!this.props.dictionary) return null;
      const {
        simplified,
        pinyin,
        english,
        definitions,
        pinned
      } = this.props.dictionary;
      // console.log("dict", this.props.dictionary);
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
            <ul>
              {definitions.map(lst => (
                <li key={JSON.stringify(lst)}>
                  {lst.english.map(elt => (
                    <span key={elt}>
                      <span onClick={this.setEnglish(elt)}>{elt}</span>
                    </span>
                  ))}
                </li>
              ))}
            </ul>
          </div>
        </div>
      );
    };
  }
);
