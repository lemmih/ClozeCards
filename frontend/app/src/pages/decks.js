import _ from 'lodash'
import React, { Component } from 'react'
import { withRouter, Redirect } from 'react-router-dom'
import { connect } from 'react-redux'
import {
  Container, Item, Loader
} from 'semantic-ui-react'
import uuid from 'uuid/v4'

import DeckHeader from '../components/deck-header'
import DeckBody from '../components/deck-body'
import DeckComments from '../components/deck-comments'
import Study from '../components/deck-study'

import {receiveDeck, fetchDeck} from '../actions/decks'
import {receiveContent, fetchContent} from '../actions/content'

import mkSlug from '../misc/slugs'

import backend from '../backend'

// states:
//   not fetched => doesn't have key
//   fetching    => true
//   fetched     => obj
//   missing     => false
function toViewDeckProps(store, ownProps) {
  const slug = ownProps.slug;
  const deck = store.decksBySlug.get(slug);
  const content = deck ? store.content.get(deck.contentId) : null;
  return {
    deck: deck,
    content: content
  };
}
export const ViewDeck = connect(toViewDeckProps)(class ViewDeck extends Component {
  constructor(props) {
    super(props);
    const {study, notes} = props;
    this.state = { editable: false, study, notes };
    // console.log('ViewDeck Constructor', !!study, !!notes);
  }
  componentWillReceiveProps = (nextProps) => {
    // console.log('Will receive', nextProps);
    if( nextProps.study === undefined && nextProps.notes === undefined)
      this.setState({study: false, notes: false});
    if(nextProps.study !== undefined)
      this.setState({study: nextProps.study});
    if(nextProps.notes !== undefined)
      this.setState({notes: nextProps.notes});
  }

  handleEdit = () => {
    this.setState({ editable: true });
  }
  handleChange = deck => {
    this.setState({ deck });
  }
  handleSave = () => {
    const deck = this.state.deck || this.props.deck;
    const slug = mkSlug(deck.title);
    const slugs = _.uniq([slug].concat(deck.slugs));
    const contentId = uuid();


    backend.relay(receiveContent(contentId, this.bodyRef.deckEditorRaw()));
    backend.relay(receiveDeck({...deck, slugs, contentId}));

    this.setState({ editable: false });
    this.bodyRef.setState({editNotes: false});
    // this.props.history.push('/decks/'+slug);
  }
  handleDiscard = () => {
    this.bodyRef.initialize();
    this.setState({ editable: false, deck: undefined });
  }
  handleBodyRef = (ref) => this.bodyRef = ref;

  componentDidMount() {
    if( _.isUndefined(this.props.deck) ) {
      backend.relay(fetchDeck(this.props.slug));
    }
    if(_.isPlainObject(this.props.deck)) {
      if( _.isUndefined(this.props.content) ) {
        backend.relay(fetchContent(this.props.deck.contentId));
      }
    }
  }
  componentDidUpdate() {
    // console.log('update', this.props.deck, this.props.content);
    if( this.props.deck === undefined ) {
      backend.relay(fetchDeck(this.props.slug));
    }
    // console.log('update', this.props.deck, this.props.content);
    if(_.isPlainObject(this.props.deck)) {
      if( _.isUndefined(this.props.content) ) {
        backend.relay(fetchContent(this.props.deck.contentId));
      }
    }
  }

  render() {
    const {deck, content, study} = this.props;
    const hasDeck = _.isPlainObject(deck);
    const hasContent = _.isPlainObject(content)
    if( this.props.deck === 'failed')
      return <Redirect to="/"/>
    if( !hasDeck )
      return (<Loader active/>)
    const realDeck = this.state.deck || deck;
    return(
      <div>
        <Container style={{paddingTop: "2em"}}>
          <Item.Group>
            <DeckHeader
              deck={realDeck}
              editable={this.state.editable}
              onEdit={this.handleEdit}
              onChange={this.handleChange}
              notesVisible={this.state.notes}
              mayEdit={true && !this.state.notes && !this.state.study}/>
          </Item.Group>

          { hasContent
          ? <DeckBody
            deckContent={this.props.content}
            editable={this.state.editable}
            showAnnotations={this.state.notes}
            onSave={this.handleSave}
            onDiscard={this.handleDiscard}
            ref={this.handleBodyRef} />
          : <Loader active/> }

          { false && <DeckComments/>}
        </Container>
        { study && <Study deckId={deck.id}/> }
      </div>
    );
  }
})

function toNewDeckProps(store) {
  return {
    decks: store.decks
  };
}

const NewDeck = withRouter(connect(toNewDeckProps)(class NewDeck extends Component {
  state = {};
  onSave = () => {
    const {deck} = this.state;
    const slug = mkSlug(deck.title);
    const contentId = uuid();
    const deckId = uuid();

    backend.relay(receiveContent(contentId, this.bodyRef.deckEditorRaw()));
    backend.relay(receiveDeck({...deck, id: deckId, slugs: [slug], contentId, nLikes: 0, nComments: 0, dirty: true, processing: true}));

    this.props.history.push('/decks/'+deckId);
  }
  handleChange = deck => {
    this.setState({ deck });
  }
  handleBodyRef = (ref) => this.bodyRef = ref;

  render = () => {
    const deck = this.state.deck || {title: '', tags: []};
    return(
      <div>
        <Container style={{paddingTop: "2em"}}>
          <Item.Group>
            <DeckHeader
              deck={deck}
              onChange={this.handleChange}
              mayEdit={true}
              editable={true}
              />
          </Item.Group>
          <DeckBody
            ref={this.handleBodyRef}
            editable={true}
            showAnnotations={false}
            onSave={this.onSave}
            />
        </Container>
      </div>
    );
  }
}))

export { NewDeck };
