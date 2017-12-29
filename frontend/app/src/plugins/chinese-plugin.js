import _ from "lodash";
import axios from "axios";
import { Set, is } from "immutable";
import React from "react";
import { connect } from "react-redux";
import {
  EditorState,
  Modifier,
  SelectionState,
  DraftEditorBlock
} from "draft-js";

import { pinDictionary } from "../actions/dictionary";

import { showDictionary, hideDictionary } from "../store-interface";

function applyEntity(contentState, selectionState, entityKey) {
  const state1 = Modifier.applyEntity(contentState, selectionState, entityKey);
  return state1.merge({
    selectionBefore: contentState.getSelectionBefore(),
    selectionAfter: contentState.getSelectionAfter()
  });
}

function handleChinese(contentBlock, callback, contentState) {
  contentBlock.findEntityRanges(character => {
    const entityKey = character.getEntity();
    if (entityKey === null) return false;
    return (
      !character.hasStyle("HIDDEN") &&
      contentState.getEntity(entityKey).getType() === "CHINESE"
    );
  }, callback);
}

class HandleChinese extends React.PureComponent {
  dictEntry = () => {
    const { decoratedText } = this.props;
    return {
      simplified: decoratedText,
      pinyin: null,
      english: null,
      definitions: null
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
  render = () => {
    return (
      <span
        className="chinese"
        data-offset-key={this.props.offsetKey}
        onClick={this.pinDict}
        onMouseEnter={this.showDict}
        onMouseLeave={this.hideDict}
      >
        {this.props.children}
      </span>
    );
  };
}

function handleHidden(contentBlock, callback, contentState) {
  // There's a bug in how draftjs check for style equality. It uses (===) but
  // that doesn't work on OrderedSets. Should use Immutable.is.
  const charList = contentBlock.getCharacterList();
  var cursor = 0;
  charList.reduce((value, nextValue, nextIndex) => {
    if (!is(value.getStyle(), nextValue.getStyle())) {
      if (value.hasStyle("HIDDEN")) {
        callback(cursor, nextIndex);
      }
      cursor = nextIndex;
    }
    return nextValue;
  });
  charList.last().hasStyle("HIDDEN") && callback(cursor, charList.count());

  // contentBlock.findStyleRanges(
  //   character => {
  //     console.log("style", character.getStyle());
  //     // return true;
  //     return character.hasStyle("HIDDEN");
  //   },
  //   (start, end) => {
  //     console.log("callback", start, end);
  //     callback(start, end);
  //   }
  // );
}
const RenderHidden = props => {
  return (
    <span className="hidden" data-offset-key={props.offsetKey}>
      {props.children}
    </span>
  );
};

class AudioIndexBlock extends React.Component {
  render = () => {
    // console.log(this.props);
    // return null;
    // return DraftEditorBlock(this.props);
    return (
      <div>
        <DraftEditorBlock {...this.props} />
      </div>
    );
    // return <div>{this.props.children}</div>;
  };
}

export default () => {
  var dirtyBlocks = Set();
  const markChineseEntity = (contentState, blockKey, range) => {
    const selection = SelectionState.createEmpty(blockKey)
      .set("anchorOffset", range.start)
      .set("focusOffset", range.end);
    const withEntity = contentState.createEntity("CHINESE", "MUTABLE", {});
    const entityKey = withEntity.getLastCreatedEntityKey();
    const applied = applyEntity(withEntity, selection, entityKey);
    return applied;
  };
  const fetchEntities = _.debounce(editor => {
    const content = editor.getEditorState().getCurrentContent();
    var req = {};
    dirtyBlocks.forEach(key => {
      const block = content.getBlockForKey(key);
      if (block) req[key] = block.getText();
    });
    // console.log('Request', req);
    dirtyBlocks = Set();
    // this.setState({...this.state, dirtyBlocks: Set()});
    // console.log('Fetching entries: ', req, editor.getEditorRef().props.placeholder);
    axios
      .post("/api/segmentation/", req)
      .then(response => {
        //  console.log('Got response', response.data);
        var contentState = editor.getEditorState().getCurrentContent();
        Object.keys(response.data).forEach(key => {
          if (!dirtyBlocks.has(key) && contentState.getBlockForKey(key)) {
            // Clear all entities in block.
            const entireBlock = SelectionState.createEmpty(key).set(
              "focusOffset",
              req[key].length
            );
            contentState = applyEntity(contentState, entireBlock, null);
            response.data[key].forEach(range => {
              //  console.log('Adding entity', key, range);
              contentState = markChineseEntity(contentState, key, range);
            });
          } else {
            //  console.log('Got dirty in transfer.');
          }
        });
        const newState = EditorState.set(editor.getEditorState(), {
          currentContent: contentState,
          lastChangeType: "mark-chinese"
        });
        editor.setEditorState(newState);
      })
      .catch(error => {
        console.log("Got error", error);
      });
  }, 250);
  return {
    blockRendererFn: block => {
      if (block.getType() === "audio-index") {
        return {
          component: AudioIndexBlock, //DraftEditorBlock,
          editable: false
        };
      }
    },
    // blockRenderMap: Map({
    //   "audio-index": {
    //     element: "div",
    //     wrapper: <AudioIndexBlock />
    //   }
    // }),
    decorators: [
      {
        strategy: handleChinese,
        component: connect()(HandleChinese)
      },
      {
        strategy: handleHidden,
        component: RenderHidden
      }
    ],
    onChange: (editorState, editor) => {
      const blockMap = editorState.getCurrentContent().getBlockMap();
      const before = editorState.getCurrentContent().getSelectionBefore();
      const after = editorState.getCurrentContent().getSelectionAfter();
      const now = editorState.getSelection();
      const change = editorState.getLastChangeType();
      const unchanged =
        editorState.getCurrentContent() ===
        editor.getEditorState().getCurrentContent();
      var newDirtyBlocks = [];

      if (change !== null && change !== "mark-chinese" && !unchanged) {
        if (before === after) {
          // console.log('Selection change', now.anchorKey);
        } else if (before.anchorKey === after.anchorKey) {
          const sel = SelectionState.createEmpty(after.anchorKey).merge({
            anchorOffset: before.getFocusOffset(),
            focusOffset: after.getFocusOffset()
          });
          const contentState = applyEntity(
            editorState.getCurrentContent(),
            sel,
            null
          );
          editorState = EditorState.set(editorState, {
            currentContent: contentState
          });
          // console.log('Single block:', now.anchorKey, after.focusOffset, before.focusOffset);
          newDirtyBlocks.push(now.anchorKey);
        } else {
          if (blockMap.has(before.anchorKey)) {
            // console.log('Multi block.', now.anchorKey, before.anchorKey, after.anchorKey);
            blockMap
              .skipUntil((_, k) => k === before.anchorKey)
              .takeUntil((_, k) => k === after.anchorKey)
              .forEach((_, k) => {
                newDirtyBlocks.push(k);
              });
            newDirtyBlocks.push(after.anchorKey);
          } else {
            // console.log('Deleted blocks', now.anchorKey, before.anchorKey, after.anchorKey);
            newDirtyBlocks.push(now.anchorKey);
          }
        }
      }

      dirtyBlocks = dirtyBlocks.union(Set(newDirtyBlocks));
      // console.log('Dirty blocks', dirtyBlocks.size, newDirtyBlocks, change, unchanged);
      if (!dirtyBlocks.isEmpty()) fetchEntities(editor);

      // editor.setEditorState(editorState);
      return editorState;
    }
  };
};
