import React, { PureComponent } from "react";
import { Button } from "semantic-ui-react";
import { RichUtils, EditorState } from "draft-js";

const mkInline = (activeStyles, setEditorState) =>
  class extends PureComponent {
    toggle = event => {
      event.preventDefault();
      setEditorState(state =>
        RichUtils.toggleInlineStyle(state, this.props.type)
      );
    };
    render = () => {
      const active = activeStyles.has(this.props.type);
      return (
        <Button
          toggle
          onClick={this.toggle}
          active={active}
          icon={this.props.icon}
        >
          {this.props.children}
        </Button>
      );
    };
  };

const mkBlock = (block, setEditorState) =>
  class extends PureComponent {
    toggle = event => {
      event.preventDefault();
      setEditorState(state =>
        RichUtils.toggleBlockType(state, this.props.blockType)
      );
    };
    render = () => {
      const active = block.getType() === this.props.blockType;
      return (
        <Button
          toggle
          onClick={this.toggle}
          active={active}
          icon={this.props.icon}
        >
          {this.props.children}
        </Button>
      );
    };
  };

export default (editorState, setEditorState) => {
  const activeStyles = editorState.getCurrentInlineStyle();
  const startKey = editorState.getSelection().getStartKey();
  const block = editorState.getCurrentContent().getBlockForKey(startKey);
  const updEditorState = fn => setEditorState(fn(editorState));
  return {
    Inline: mkInline(activeStyles, updEditorState),
    Block: mkBlock(block, updEditorState)
  };
};
