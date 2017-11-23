import _ from "lodash";
import React, { Component, PureComponent } from "react";
// import {
//   Popup
// } from 'semantic-ui-react'

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
    const { blocks, english, active, showEnglish } = this.props;
    const done = active >= blocks.length;
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
        <div style={innerStyle}>
          {blocks.map((block, idx) => (
            <Block
              onAnswer={this.props.onAnswer}
              onSpace={this.props.onSpace}
              onEscape={this.props.onEscape}
              showPinyin={done || this.props.showPinyin}
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

class Block extends PureComponent {
  showDict = () => {
    console.log("Enter");
  };
  hideDict = () => {
    console.log("Leave");
  };
  render() {
    const { block, active, showPinyin } = this.props;
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
    const pinyin = _.uniq(
      block.definitions.map(def => def.pinyin.toLowerCase())
    );

    const renderPinyin = <ul>{pinyin.map(p => <li key={p}>{p}</li>)}</ul>;

    if (active)
      return (
        <div style={blockStyle}>
          <div>
            {showPinyin ? <div>{renderPinyin}</div> : <div>&nbsp;</div>}
            <div>
              <PinyinInput
                onEnter={this.props.onAnswer}
                onSpace={this.props.onSpace}
                onEscape={this.props.onEscape}
                style={inputStyle}
                placeholder={txt}
              />
            </div>
          </div>
        </div>
      );

    return (
      <div style={blockStyle}>
        {showPinyin || (block.isGap && this.props.completed) ? (
          <div>{renderPinyin}</div>
        ) : (
          <div>&nbsp;</div>
        )}
        <div onMouseEnter={this.showDict} onMouseLeave={this.hideDict}>
          {block.simplified}
        </div>
        <div>&nbsp;</div>
      </div>
    );
  }
}

export default ClozeSentence;
