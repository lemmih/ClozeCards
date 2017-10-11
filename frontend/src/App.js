import _ from 'lodash'
import React, { Component } from 'react'
import { EditorState, convertFromRaw, RichUtils } from 'draft-js'
import Editor from 'draft-js-plugins-editor'
import './App.css';
import {ResizeSensor} from 'css-element-queries'
import createChinesePlugin from './chinese-plugin.js'
import alignContent from './align.js'

class App extends Component {
  constructor(props) {
    super(props);
    const blocks = convertFromRaw({
      blocks: [
        { text: 'This might be some text written by a teacher. ' +
                'Lines can be quite long and it is important ' +
                'that annotations line up with the content.'
        , type: 'unstyled'},
        { text: 'This is further complicated by things like lists and images. ' +
                'Even Chinese characters pose a problem because they are taller ' +
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
        { text: 'Line 1你好吗？'
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
      editorState: EditorState.createWithContent(blocks),
      annState: EditorState.createWithContent(annBlocks)
    };
    this.focus = () => this.refs.editor.focus();
    this.focusAnn = () => this.refs.ann.focus();
    this.textPlugins = [ createChinesePlugin() ];
    this.annPlugins = [ createChinesePlugin() ];
  }


  handleKeyCommandA = (command, editorState) => {
    const newState = RichUtils.handleKeyCommand(editorState, command);
    if (newState) {
      this.setState({editorState: newState});
      return 'handled';
    }
    return 'not-handled';
  }
  handleKeyCommandB = (command, editorState) => {
    const newState = RichUtils.handleKeyCommand(editorState, command);
    if (newState) {
      this.setState({annState: newState});
      return 'handled';
    }
    return 'not-handled';
  }

  alignText = _.throttle(() => alignContent(this.refs.editor.editor, this.refs.ann.editor), 250)

  componentDidMount = () => {
    this.alignText();
    const self = this;
    this.rightSensor = new ResizeSensor(this.refs.ann.editor.refs.editor, () => {
      self.alignText();
    });
    this.leftSensor = new ResizeSensor(this.refs.editor.editor.refs.editor, () => {
      self.alignText();
    });
  }
  componentWillUnmount = () => {
    this.rightSensor.detach();
    this.leftSensor.detach();
  }

  render() {
    return (
      <div className="App">
        <p>Editor with Chinese support.</p>
        <div onClick={this.focus}>
          <Editor
            editorState={this.state.editorState}
            handleKeyCommand={this.handleKeyCommandA}
            onChange={(editorState) => this.setState({editorState})}
            plugins={this.textPlugins}
            placeholder="Left placeholder"
            ref="editor"
          />
        </div>
        <div onClick={this.focusAnn}>
          <Editor
            editorState={this.state.annState}
            onChange={(annState) => this.setState({annState})}
            handleKeyCommand={this.handleKeyCommandB}
            plugins={this.annPlugins}
            placeholder="Right placeholder"
            ref="ann"
          />
        </div>
      </div>
    );
  }
}

export default App;
