import React, { Component } from "react";

class LandingPage extends Component {
  render = () => {
    return (
      <div>
        <p>
          <center>
            <h1>Public Beta.</h1>
            <h2>
              Many features do not work and any content you add to the site may
              be lost
            </h2>
          </center>
        </p>
        <p>
          Features of this beta version includes:
          <ul>
            <li>
              <b>User created study decks.</b>
              <p>
                New decks are added using a rich content editor and the
                vocabulary can be studied using 40,000 example sentences.
              </p>
            </li>
            <li>
              <b>Notes.</b>
              <p>
                All decks (even those not created by you) can be annotated with
                notes. These notes are aligned with the context in the deck such
                they'll always match up line-by-line, regardless of window size
                or formatting. Furthermore, any chinese written in the notes
                will trigger the built-in dictionary, just like in the decks.
              </p>
            </li>
            <li>
              <b>Listening exercises.</b>
              <p>
                Practice your comprehension by listening to each practice
                sentence and then write down the characters (or pinyin).
              </p>
            </li>
            <li>
              <b>Reading exercises.</b>
              <p>
                Memorize words quickly by seeing them in a familiar context (ie.
                in sentence that only use words you already know) and write
                their pronounciation (in pinyin) to continue.
              </p>
            </li>
            <li>
              <b>Study mode.</b>
              <p>
                Push your limits by practicing new words or words you're just
                about to forget. New words will are introduced until you reach a
                20% recall failure rate.
              </p>
            </li>
            <li>
              <b>Review mode.</b>
              <p>
                <a href="https://en.wikipedia.org/wiki/Overlearning">
                  Overlearn
                </a>{" "}
                by studying words that you haven't seen in a long time,
                regarless of whether the learning algorithm believes you've
                about to forget them. This mode is relaxing, builds mastery, and
                improves your comprehension by showing you known words in
                completely new sentences.
              </p>
            </li>
            <li>
              <b>Speed vs. Mastery mode</b>
              <p>
                In speed mode, you are only quizzed in the words you're actively
                learning right now. This makes it very quick to review large
                quantities of vocabulary but with less attention paid to the
                broader context.
              </p>
              <p>
                In mastery mode, you are quizzed in the entire sentence, forcing
                you to read it and notice the context of the words you're
                learning. An added benefit to this mode is that forgotten words
                are quickly discovered even if they're not part of your active
                set.
              </p>
            </li>
          </ul>
        </p>
      </div>
    );
  };
}

export default LandingPage;
