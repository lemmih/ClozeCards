import _ from 'lodash'
import axios from 'axios'
import { Set } from 'immutable'
import React, { Component } from 'react';
import { Editor, EditorState, convertToRaw, convertFromRaw, RichUtils,
         CompositeDecorator, Modifier, SelectionState } from 'draft-js'
import './App.css';

class App extends Component {
  constructor(props) {
    super(props);
    const compositeDectorator = new CompositeDecorator([
      {
        strategy: handleChinese,
        component: HandleSpan
      }
    ]);
    this.state = {
      editorState: EditorState.createEmpty(compositeDectorator),
      dirtyBlocks: Set()
    };
  }

  markChineseEntity = (contentState, blockKey, range) => {
    // const prevSelection = editorState.getSelection();
    const selection = SelectionState.createEmpty(blockKey).set('anchorOffset', range.start).set('focusOffset', range.end);
    // const contentState = editorState.getCurrentContent();
    const withEntity = contentState.createEntity('CHINESE', 'MUTABLE', {});
    const entityKey = withEntity.getLastCreatedEntityKey();
    const applied = Modifier.applyEntity(withEntity, selection, entityKey);
    // const newState = EditorState.push(this.state.editorState, applied, 'apply-entity');
    // const withSel = EditorState.forceSelection(newState, prevSelection);
    return applied;
  }

  fetchEntities = _.debounce((dirty) => {
    const self = this;
    const content = this.state.editorState.getCurrentContent();
    var req = {};
    dirty.forEach(key => {
      const block = content.getBlockForKey(key);
      if(block)
        req[key] = block.getText();
    });
    console.log('Request', req);
    this.setState({...this.state, dirtyBlocks: Set()});
    axios.post('http://localhost:8000/segmentation/',req)
         .then((response) => {
           console.log('Got response', response.data);
           var contentState = self.state.editorState.getCurrentContent();
           Object.keys(response.data).forEach(key => {
             if(!self.state.dirtyBlocks.has(key)) {
               // Clear all entities in block.
               const entireBlock = SelectionState.createEmpty(key).set('focusOffset', req[key].length);
               contentState = Modifier.applyEntity(contentState, entireBlock, null);
               response.data[key].forEach(range => {
                //  console.log('Adding entity', key, range);
                 contentState = self.markChineseEntity(contentState, key, range);
               });

             } else {
               console.log('Got dirty in transfer.');
             }
           });
           const newState = EditorState.set(this.state.editorState, {currentContent: contentState});
           self.onChange(newState);
         })
         .catch((error) => {
           console.log('Got error', error);
         });
    // console.log('Fetching entries: ', dirty.size);

  }, 250)

  onChange = (editorState) => {
    const blockMap = editorState.getCurrentContent().getBlockMap();
    const blockKey = editorState.getSelection().getStartKey();
    const block = editorState.getCurrentContent().getBlockForKey(blockKey);
    const before = editorState.getCurrentContent().getSelectionBefore();
    const after = editorState.getCurrentContent().getSelectionAfter();
    const now = editorState.getSelection();
    const change = editorState.getLastChangeType();
    const unchanged = editorState.getCurrentContent() == this.state.editorState.getCurrentContent();
    var dirtyBlocks = [];

    if( change !== null && !unchanged) {
      // console.log('Dirty:', this.state.dirtyBlocks);
      // console.log('Change type:', change);
      // editorState.getCurrentContent().getBlockMap().forEach((value, key) => {
      //   console.log('Key', key);
      // });
      if( before === after ) {
        // console.log('Selection change', now.anchorKey);
      } else if( before.anchorKey === after.anchorKey ) {
        // console.log('Single block:', now.anchorKey, after.anchorKey);
        dirtyBlocks.push(now.anchorKey);
      } else {
        if(blockMap.has(before.anchorKey)) {
          // console.log('Multi block.', now.anchorKey, before.anchorKey, after.anchorKey);
          blockMap.skipUntil((_,k) => k === before.anchorKey)
                  .takeUntil((_,k) => k === after.anchorKey)
                  .forEach((_,k) => {
                    dirtyBlocks.push(k);
                  });
          dirtyBlocks.push(after.anchorKey);
        } else {
          // console.log('Deleted blocks', now.anchorKey, before.anchorKey, after.anchorKey);
          dirtyBlocks.push(now.anchorKey);
        }
      }
      // console.log('Dirty', dirtyBlocks);
    }
    // console.log('Before', editorState.getCurrentContent().getSelectionBefore());
    // console.log('After', editorState.getCurrentContent().getSelectionAfter());
    // console.log('Selection', editorState.getSelection());
    // console.log('State', block.getType(), block.getText());
    // console.log('Block', JSON.stringify(block));
    // console.log('State', JSON.stringify(convertToRaw(editorState.getCurrentContent())));
    const newDirty = this.state.dirtyBlocks.union(Set(dirtyBlocks));
    if(!newDirty.isEmpty())
      this.fetchEntities(newDirty);
    this.setState({editorState: editorState, dirtyBlocks: newDirty});
    // this.setState({editorState: EditorState.createWithContent(convertFromRaw(convertToRaw(editorState.getCurrentContent())))});
  };
  handkeKeyCommand = (command) => {
    const newState = RichUtils.handleKeyCommand(this.state.editorState, command);
    if (newState) {
      this.onChange(newState);
      return 'handled';
    }
    return 'not-handled';
  }
  onUnderlineClick = () => {
    const contentState = this.state.editorState.getCurrentContent();
    const withEntity = contentState.createEntity('CHINESE', 'MUTABLE', {});
    const entityKey = withEntity.getLastCreatedEntityKey();
    const applied = Modifier.applyEntity(withEntity, this.state.editorState.getSelection(), entityKey)
    const newState = EditorState.push(this.state.editorState, applied, 'apply-entity');
    this.onChange(newState);
    // this.onChange(RichUtils.toggleInlineStyle(this.state.editorState, 'UNDERLINE'));
  }
  render() {
    return (
      <div className="App">
        <p>Editor with Chinese support.</p>
        <div>
          <button onClick={this.onUnderlineClick}>Underline</button>
          <Editor
            editorState={this.state.editorState}
            handleKeyCommand={this.handleKeyCommand}
            onChange={this.onChange}
            placeholder="Enter text here..."
          />
        </div>
      </div>
    );
  }
}

function handleChineseRegex(contentBlock, callback, contentState) {
  const text = contentBlock.getText();
  let matchArr, start;
  const re = /HI/g;
  while ((matchArr=re.exec(text)) !== null) {
    start = matchArr.index;
    console.log('Regex', start, matchArr[0].length);
    callback(start, start + matchArr[0].length);
  }
}
function handleChinese(contentBlock, callback, contentState) {
  contentBlock.findEntityRanges(
    (character) => {
      const entityKey = character.getEntity();
      if (entityKey===null)
        return false
      return true
    }, callback);
}

// class HandleSpan extends Component {
//   style={ backgroundColor: 'lightblue' };
//   enterEvent() {
//     console.log('Entering');
//     return true;
//   }
//   render() {
//     return <span
//               onMouseEnter={this.enterEvent}
//               style={this.style}
//               className="chinese"
//               data-offset-key={this.props.offsetKey}>{this.props.children}</span>;
//   }
// }

const HandleSpan = (props) => {
  const style={ };
  return (
    <span
      onMouseOver={() => console.log('Enter', props)}
      style={style}
      className="chinese"
      data-offset-key={props.offsetKey}>{props.children}</span>
  );
};

export default App;
