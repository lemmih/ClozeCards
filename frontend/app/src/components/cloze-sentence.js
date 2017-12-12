import _ from "lodash";
import React, { Component, PureComponent } from "react";
import { connect } from "react-redux";
// import {
//   Popup
// } from 'semantic-ui-react'

import {
  showDictionary,
  hideDictionary,
  pinDictionary
} from "../actions/dictionary";

import PinyinInput from "./pinyin-input";

function blockId(block) {
  if (_.isString(block)) return block;
  return block.simplified;
}

// props:
//   blocks: [block]
//   english: string
//   showEnglish: bool
//   showAnswer: bool
//   onAnswer: function
//   active: int
//
class ClozeSentence extends Component {
  render() {
    const { blocks, english, active, showEnglish, mode } = this.props;
    const done = active >= blocks.length;
    const showPinyin = idx =>
      done ||
      (this.props.showPinyin && idx === active) ||
      (mode === "ship" && idx < active);
    const outerStyle = {
      position: "relative",
      float: "left",
      left: "50%"
    };
    const innerStyle = {
      position: "relative",
      float: "left",
      left: "-50%"
    };
    const englishStyle = {
      clear: "both"
    };
    return [
      <div key="chinese" className="blocks" style={outerStyle}>
        <div className="block-container" style={innerStyle}>
          {blocks.map((block, idx) => (
            <Block
              onAnswer={this.props.onAnswer}
              onShiftEnter={this.props.onShiftEnter}
              onSpace={this.props.onSpace}
              onEscape={this.props.onEscape}
              mode={mode}
              type={this.props.type}
              showPlaceholder={this.props.showPlaceholder}
              showPinyin={showPinyin(idx)}
              active={idx === active}
              completed={idx < active}
              key={idx.toString() + "-" + blockId(block)}
              block={block}
            />
          ))}
        </div>
      </div>,
      showEnglish > 0 ? (
        <div key="english" style={englishStyle}>
          {english}
        </div>
      ) : (
        <div key="english" style={englishStyle}>
          &nbsp;
        </div>
      )
    ];
  }
}

function blockSize(block) {
  if (_.isPlainObject(block)) {
    let min = block.simplified.length;
    _.forEach(block.definitions, definition => {
      min = Math.max(min, Math.floor(definition.pinyin.length / 2) + 1);
    });
    return min;
  }
}

const Block = connect()(
  class Block extends PureComponent {
    dictEntry = () => {
      const { simplified, pinyin, english, definitions } = this.props.block;
      return {
        simplified: simplified,
        pinyin: pinyin,
        english: english,
        definitions: definitions
      };
    };
    showDict = () => {
      this.props.dispatch(showDictionary(this.dictEntry()));
    };
    pinDict = () => {
      this.props.dispatch(pinDictionary(this.dictEntry()));
    };
    hideDict = () => {
      this.props.dispatch(hideDictionary());
    };
    render() {
      const {
        block,
        active,
        showPinyin,
        showPlaceholder,
        mode,
        type,
        completed
      } = this.props;
      const soundType = type === "sound";
      const rocketMode = mode === "rocket";
      const inputStyle = {
        border: "0px",
        borderBottom: "1px solid black",
        width: blockSize(block) + "em"
      };
      const blockStyle = {
        float: "left"
      };
      if (_.isString(block)) {
        return (
          <div style={blockStyle}>
            <div>&nbsp;</div>
            <div>{block}</div>
          </div>
        );
      }

      const txt = block.simplified;
      const renderPinyin = block.pinyin;
      const blanks = Array(block.simplified.length)
        .fill("_")
        .join(" ");

      if (active)
        return (
          <div style={blockStyle}>
            {showPinyin ? <div>{renderPinyin}</div> : <div>&nbsp;</div>}
            <div>
              <PinyinInput
                onEnter={this.props.onAnswer}
                onShiftEnter={this.props.onShiftEnter}
                onSpace={this.props.onSpace}
                onEscape={this.props.onEscape}
                style={inputStyle}
                placeholder={showPlaceholder ? txt : blanks}
              />
            </div>
          </div>
        );

      return (
        <div style={blockStyle}>
          {showPinyin || (block.isGap && completed) ? (
            <div>{renderPinyin}</div>
          ) : (
            <div>&nbsp;</div>
          )}
          {!soundType || completed || (rocketMode && !block.isGap) ? (
            <div
              onClick={this.pinDict}
              onMouseEnter={this.showDict}
              onMouseLeave={this.hideDict}
            >
              {block.simplified}
            </div>
          ) : (
            <div style={{ width: block.simplified.length + "em" }}>
              {blanks}
            </div>
          )}

          <div>&nbsp;</div>
        </div>
      );
    }
  }
);

export default ClozeSentence;
