import _ from 'lodash'
import axios from 'axios'
import { Set, Map } from 'immutable'
import React, { Component } from 'react';
import { Editor, EditorState, convertToRaw, convertFromRaw, RichUtils,
         CompositeDecorator, Modifier, SelectionState,
         DefaultDraftBlockRenderMap, DraftEditorBlock } from 'draft-js'
import './App.css';
import $ from 'jquery'

class AnnotationBlock extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    // return <div/>;
    return (
      <div {...this.props} contentEditable="false" className="AnnotationBlock">{this.props.children}</div>
    );
  }
}

const TEST = props => <div {...props} className="Locked"></div>;
const blockRenderMap = Map();
// const blockRenderMap = Map({
//   'unstyled-locked': {
//     element: AnnotationBlock,
//     // wrapper: TEST
//   }
// });

const extendedBlockRenderMap = DefaultDraftBlockRenderMap.merge(blockRenderMap);

function myBlockRenderer(contentBlock) {
  const type = contentBlock.getType();
  console.log('Block type', type, contentBlock.getData().toJS());
  if (type === 'unstyled-locked') {
    // console.log('Rendering locked.');
    return {
      component: DraftEditorBlock,
      // component: (props) => <div id="locked"></div>,
      editable: false
    };
  }
}

class App extends Component {
  constructor(props) {
    super(props);
    const compositeDectorator = new CompositeDecorator([
      {
        strategy: handleChinese,
        component: HandleSpan
      }
    ]);
    const blocks = convertFromRaw({
      blocks: [
        { text: 'This might be some text written by a teacher. ' +
                'Lines can be quite long and it is important ' +
                'that annotations line up with the content.'
        , type: 'unstyled'},
        { text: 'This is further complicated by things like lists and images. ' +
                'Even Chinese characters pose a problem because they are higher ' +
                'than latin letters.'
        , type: 'unordered-list-item'
        , depth: 1},
        { text: 'Here\'s a short list item'
        , type: 'unordered-list-item'
        , depth: 2},
        { text: '你好吗?'
        , type: 'unstyled' },
        { text: 'There should be a slight gap ->'
        , type: 'unstyled' },
        { text: 'There should be no gap ->'
        , type: 'unstyled' },
      ],
      entityMap: {}
    });
    const annBlocks = convertFromRaw({
      blocks: [
        { text: 'Line 1'
        , type: 'unstyled'},
        { text: 'Line 2'
        , type: 'unstyled'},
        { text: 'Line 3'
        , type: 'unstyled'},
        { text: 'Line 4'
        , type: 'unstyled' },
        { text: 'Line 5'
        , type: 'unstyled' },
        { text: 'Line 6'
        , type: 'unstyled' },
      ],
      entityMap: {}
    });
    this.state = {
      editorState: EditorState.createWithContent(blocks, compositeDectorator),
      annState: EditorState.createWithContent(annBlocks),
      dirtyBlocks: Set()
    };
    this.focus = () => this.refs.editor.focus();
    this.focusAnn = () => this.refs.ann.focus();
  }

  markChineseEntity = (contentState, blockKey, range) => {
    // const prevSelection = editorState.getSelection();
    const selection = SelectionState.createEmpty(blockKey).set('anchorOffset', range.start).set('focusOffset', range.end);
    // const contentState = editorState.getCurrentContent();
    const withEntity = contentState.createEntity('CHINESE', 'MUTABLE', {});
    const entityKey = withEntity.getLastCreatedEntityKey();
    const applied = applyEntity(withEntity, selection, entityKey);
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
    // console.log('Request', req);
    this.setState({...this.state, dirtyBlocks: Set()});
    axios.post('http://localhost:8000/segmentation/',req)
         .then((response) => {
          //  console.log('Got response', response.data);
           var contentState = self.state.editorState.getCurrentContent();
           Object.keys(response.data).forEach(key => {
             if(!self.state.dirtyBlocks.has(key)) {
               // Clear all entities in block.
               const entireBlock = SelectionState.createEmpty(key).set('focusOffset', req[key].length);
               contentState = applyEntity(contentState, entireBlock, null);
               response.data[key].forEach(range => {
                //  console.log('Adding entity', key, range);
                 contentState = self.markChineseEntity(contentState, key, range);
               });

             } else {
               console.log('Got dirty in transfer.');
             }
           });
           const newState = EditorState.set(this.state.editorState, {currentContent: contentState});
           self.setState({...self.state, editorState: newState});
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
    const unchanged = editorState.getCurrentContent() === this.state.editorState.getCurrentContent();
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
        const sel = SelectionState.createEmpty(after.anchorKey).merge({anchorOffset: before.getFocusOffset(),
                                                                  focusOffset: after.getFocusOffset()});
        const contentState = applyEntity(editorState.getCurrentContent(), sel, null);
        editorState = EditorState.set(editorState, {currentContent: contentState});
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

    const newDirty = this.state.dirtyBlocks.union(Set(dirtyBlocks));
    if(!newDirty.isEmpty())
      this.fetchEntities(newDirty);

    this.setState({editorState: editorState, dirtyBlocks: newDirty});
  };
  handkeKeyCommand = (command) => {
    const newState = RichUtils.handleKeyCommand(this.state.editorState, command);
    if (newState) {
      this.onChange(newState);
      return 'handled';
    }
    return 'not-handled';
  }

  alignTextRaw = () => {
    const editorBlocks = $(this.refs.editor.refs.editor).find('[data-editor]');
    const annBlocks = $(this.refs.ann.refs.editor).find('[data-editor]');
    const minBlocks = Math.min(editorBlocks.length, annBlocks.length);
    var dirty = false;
    let annDyn = 0;
    let editDyn = 0;
    for(var i=1; i < minBlocks; i++) {
      const editTop = editorBlocks[i].offsetTop;
      const annTop  = annBlocks[i].offsetTop;
      const prevAnn = annBlocks[i-1];
      const prevEdit = editorBlocks[i-1];
      // const prop = 'padding-top';
      const prop = 'min-height';
      // const annExtra = 0;
      // const editExtra = 0;
      const annExtra = annBlocks[i-1].children[0].offsetHeight;
      const editExtra = editorBlocks[i-1].children[0].offsetHeight;
      function getAdded(elt) {
        const minHeight = $(elt).css('min-height');
        if( minHeight === '0px' )
          return 0;
        else
          return Math.max(0,parseInt(minHeight) - elt.children[0].offsetHeight);
      }
      const annAdded = getAdded(annBlocks[i-1]);
      const editAdded = getAdded(editorBlocks[i-1]);
      const sharedAdded = Math.min(annAdded, editAdded);
      let delta = 0;

      if( annTop+annDyn < editTop+editDyn ) {
        delta = editTop-editAdded-annTop;
        $(annBlocks[i-1]).css(prop, (delta+annAdded+annExtra) + 'px')
        $(editorBlocks[i-1]).css(prop, '0px')
        // annDyn += editTop-editAdded-annTop;
      } else if( annTop+annDyn > editTop+editDyn ) {
        delta = annTop-annAdded-editTop;
        $(editorBlocks[i-1]).css(prop, (delta+editAdded+editExtra) + 'px')
        $(annBlocks[i-1]).css(prop, '0px')
        // editDyn += annTop-annAdded-editTop;
      } else {
        // $(editorBlocks[i-1]).css(prop,'0px');
        // $(annBlocks[i-1]).css(prop,'0px');
      }
      // console.log(i,annAdded, editAdded, sharedAdded, annTop, editTop, annDyn, editDyn, delta, annExtra, editExtra);
      // if( $(editorBlocks[i-1]).css(prop) !== '0px' &&
      //     $(annBlocks[i-1]).css(prop) !== '0px'
      //     ) {
      //   $(editorBlocks[i-1]).css(prop,'0px');
      //   $(annBlocks[i-1]).css(prop,'0px');
      //   dirty = true;
      // }
      // if( parseInt($(editorBlocks[i]).css(prop)) !== 0 &&
      //     parseInt($(annBlocks[i]).css(prop)) !== 0
      //     ) {
      //   $(editorBlocks[i]).css(prop,'0px');
      //   $(annBlocks[i]).css(prop,'0px');
      //   dirty = true;
      // }
    }
    // console.log('Called');
    // if(dirty)
    // _.debounce(this.alignText, 0, {leading: false});
    // if(dirty)
    //   setTimeout(this.alignTextRaw,0);
  }
  alignText = _.debounce(this.alignTextRaw, 250)

  componentDidMount = () => {
    this.alignText();
  }

  render() {
    return (
      <div className="App">
        <p>Editor with Chinese support.</p>
        <div onClick={this.focus}>
          <Editor
            editorState={this.state.editorState}
            handleKeyCommand={this.handleKeyCommand}
            onChange={this.onChange}
            // blockRenderMap={extendedBlockRenderMap}
            // blockRendererFn={myBlockRenderer}
            placeholder="Enter text here..."
            ref="editor"
          />
        </div>
        <div onClick={this.focusAnn}>
          <Editor
            editorState={this.state.annState}
            onChange={(editorState) => { this.alignText(); this.setState({...this.state, annState: editorState})}}
            placeholder="Enter text here..."
            ref="ann"
          />
        </div>
      </div>
    );
  }
}

function applyEntity(contentState, selectionState, entityKey) {
  const state1 = Modifier.applyEntity(contentState, selectionState, entityKey);
  return state1.merge({
    selectionBefore: contentState.getSelectionBefore(),
    selectionAfter: contentState.getSelectionAfter()
  });
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
const HandleSpan = (props) => {
  return (
    <span
      className="chinese"
      data-offset-key={props.offsetKey}>{props.children}</span>
  );
};

export {App,AnnotationBlock};
// export AnnotationBlock;
