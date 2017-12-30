// @flow
import type { Dispatch } from "redux";
import _ from "lodash";
import React, { PureComponent } from "react";
import { connect } from "react-redux";

import { pinDictionary } from "../actions/dictionary";
import { showDictionary, hideDictionary } from "../store-interface";
import PinyinInput from "./pinyin-input";

type EscapedBlockT = string;
type ChineseBlockT = {
  simplified: string,
  pinyin: string,
  definitions: Array<{
    pinyin: string,
    english: Array<string>
  }>,
  answers: Array<string>,
  isGap: boolean,
  isNew: boolean,
  offset: number,
  english: ?string
};
type BlockType = EscapedBlockT | ChineseBlockT;

function blockId(block: BlockType): string {
  if (typeof block === "string") return block;
  return block.simplified;
}

type ClozeProps = {
  blocks: Array<BlockType>,
  english: string,
  showEnglish: number,
  showPinyin: boolean,
  onAnswer: string => boolean,
  onShiftEnter: string => boolean,
  onSpace: string => boolean,
  onEscape: void => boolean,
  active: number,
  showPlaceholder: boolean,
  mode: "ship" | "rocket",
  type: "keyboard" | "sound"
};

class ClozeSentence extends PureComponent<ClozeProps> {
  render() {
    const { blocks, english, active, showEnglish, mode } = this.props;
    const done = active >= blocks.length;
    const showPinyin: number => boolean = idx =>
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

type BlockProps<T> = {
  onAnswer: string => boolean,
  onShiftEnter: string => boolean,
  onSpace: string => boolean,
  onEscape: void => boolean,
  mode: "ship" | "rocket",
  type: "keyboard" | "sound",
  showPlaceholder: boolean,
  showPinyin: boolean,
  active: boolean,
  completed: boolean,
  block: T
};

class Block extends PureComponent<BlockProps<BlockType>> {
  render = () => {
    const { block } = this.props;
    const blockStyle = {
      float: "left"
    };
    if (typeof block === "string")
      return (
        <div style={blockStyle}>
          <EscapedBlock block={block} />
        </div>
      );
    else
      return (
        <div style={blockStyle}>
          <ChineseBlock {...this.props} />
        </div>
      );
  };
}

type ChineseBlockProps = BlockProps<ChineseBlockT> & {
  dispatch: Dispatch
};

function blockSize(block: ChineseBlockT): number {
  let min = block.simplified.length;
  _.forEach(block.definitions, definition => {
    min = Math.max(min, Math.floor(definition.pinyin.length / 2) + 1);
  });
  return min;
}

const ChineseBlock = connect()(
  class ChineseBlock extends PureComponent<ChineseBlockProps> {
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
      showDictionary(this.dictEntry());
    };
    pinDict = () => {
      this.props.dispatch(pinDictionary(this.dictEntry()));
    };
    hideDict = () => {
      hideDictionary();
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
        width: blockSize(block).toString() + "em"
      };

      const txt = block.simplified;
      const renderPinyin = block.pinyin;
      const blanks = Array(block.simplified.length)
        .fill("_")
        .join(" ");

      if (active)
        return [
          showPinyin ? <div>{renderPinyin}</div> : <div>&nbsp;</div>,
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
        ];

      return [
        showPinyin || (block.isGap && completed) ? (
          <div>{renderPinyin}</div>
        ) : (
          <div>&nbsp;</div>
        ),
        !soundType || completed || (rocketMode && !block.isGap) ? (
          <div
            onClick={this.pinDict}
            onMouseEnter={this.showDict}
            onMouseLeave={this.hideDict}
          >
            {block.simplified}
          </div>
        ) : (
          <div style={{ width: block.simplified.length + "em" }}>{blanks}</div>
        ),
        <div>&nbsp;</div>
      ];
    }
  }
);

class EscapedBlock extends PureComponent<{ block: EscapedBlockT }> {
  render() {
    const { block } = this.props;
    return [<div>&nbsp;</div>, <div>{block}</div>];
  }
}

export default ClozeSentence;
