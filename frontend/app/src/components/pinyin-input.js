import _ from "lodash";
import React, { PureComponent } from "react";

var vowelRegex = /([āēīōūǖĀĒĪŌáéíóúǘÁÉÍÓǎěǐǒǔǚǍĚǏǑàèìòùǜÀÈÌÒaeiouüAEIO]+)/;
// jin1tian1
// jin1 tian1
// jin1 tian21
var toneMap = {
  1: ["ā", "ē", "ī", "ō", "ū", "ǖ", "Ā", "Ē", "Ī", "Ō"],
  2: ["á", "é", "í", "ó", "ú", "ǘ", "Á", "É", "Í", "Ó"],
  3: ["ǎ", "ě", "ǐ", "ǒ", "ǔ", "ǚ", "Ǎ", "Ě", "Ǐ", "Ǒ"],
  4: ["à", "è", "ì", "ò", "ù", "ǜ", "À", "È", "Ì", "Ò"],
  5: ["a", "e", "i", "o", "u", "ü", "A", "E", "I", "O"]
};

function findAccent(txt) {
  for (var key = 1; key < 5; key++) {
    var idx = txt.search("[" + toneMap[key].join("") + "]");
    if (idx !== -1) return idx;
  }
  return -1;
}
// function stripAccent(txt, idx) {
//   for(key in toneMap) {
//     for(subkey in toneMap[key]) {
//       if(txt[idx] == )
//     }
//   }
// }
function setAccent(txt, idx, accent) {
  for (let key in toneMap) {
    for (let subkey in toneMap[key]) {
      if (txt[idx] === toneMap[key][subkey]) {
        return txt.replace(txt[idx], toneMap[accent][subkey]);
      }
    }
  }
  return txt;
}
function vowelPosition(txt) {
  var n = txt.search("[ae]");
  if (n !== -1) return n;
  n = txt.search("ou");
  if (n !== -1) return n;
  var firstPos = -1;
  for (var i = 0; i < txt.length; i++) {
    if (/[aoeiuü]/.test(txt[i])) {
      if (firstPos === -1) {
        firstPos = i;
      } else {
        return i;
      }
    }
  }
  return firstPos;
}

function transformPinyin(txt, pos, accent) {
  // 1. split text in fragments
  // 2. go through the fragments last to first
  // 3. skip fragments after pos
  // 4. if a fragment has an accent, change it, quit
  // 5. if a fragment has vowels, change the right one, quit
  var fragments = txt.split(vowelRegex);
  for (var i = fragments.length - 1, n = txt.length; i >= 0; i--) {
    n -= fragments[i].length;
    if (n >= pos) continue; // rule 3
    // console.log('Looking at fragment', fragments[i]);
    var accentPos = findAccent(fragments[i]);
    if (accentPos !== -1) {
      fragments[i] = setAccent(fragments[i], accentPos, accent);
      // console.log('has accent');
      break;
    }
    var vowelPos = vowelPosition(fragments[i]);
    if (vowelPos !== -1) {
      // console.log('has vowel');
      fragments[i] = setAccent(fragments[i], vowelPos, accent);
      break;
    }
  }
  return fragments.join("");
}

// Asterisks determine the position of the accent in pīnyīn vowel clusters
var accentsMap = {
  iao: "ia*o",
  uai: "ua*i",
  ai: "a*i",
  ao: "a*o",
  ei: "e*i",
  ia: "ia*",
  ie: "ie*",
  io: "io*",
  iu: "iu*",
  Ai: "A*i",
  Ao: "A*o",
  Ei: "E*i",
  ou: "o*u",
  ua: "ua*",
  ue: "ue*",
  ui: "ui*",
  uo: "uo*",
  ve: "üe*",
  Ou: "O*u",
  a: "a*",
  e: "e*",
  i: "i*",
  o: "o*",
  u: "u*",
  ü: "ü*",
  A: "A*",
  E: "E*",
  O: "O*"
};

// Vowels to replace with their accented froms
var vowels = ["a*", "e*", "i*", "o*", "u*", "ü*", "A*", "E*", "O*"];

// Accented characters for each of the four tones
var pinyin = {
  1: ["ā", "ē", "ī", "ō", "ū", "ǖ", "Ā", "Ē", "Ī", "Ō"],
  2: ["á", "é", "í", "ó", "ú", "ǘ", "Á", "É", "Í", "Ó"],
  3: ["ǎ", "ě", "ǐ", "ǒ", "ǔ", "ǚ", "Ǎ", "Ě", "Ǐ", "Ǒ"],
  4: ["à", "è", "ì", "ò", "ù", "ǜ", "À", "È", "Ì", "Ò"],
  5: ["a", "e", "i", "o", "u", "ü", "A", "E", "I", "O"]
};

// The replacer function
var pinyinReplace = function(match) {
  // Extract the tone number from the match
  var toneNumber = match.substr(-1, 1);

  // Extract just the syllable
  var word = match.substring(0, match.indexOf(toneNumber));

  // Put an asterisk inside of the first found vowel cluster
  for (var val in accentsMap) {
    if (word.search(val) !== -1) {
      word = word.replace(new RegExp(val), accentsMap[val]);
      break;
    }
  }

  // Replace the asterisk’d vowel with an accented character
  for (let i = 0; i < 10; i++)
    word = word.replace(vowels[i], pinyin[toneNumber][i]);

  // Return the result
  return word;
};

export default class PinyinInput extends PureComponent {
  handleKeyDown = e => {
    if (e.keyCode === 27) {
      if (this.props.onEscape()) {
        e.preventDefault();
      }
    }
  };
  handleKeyPress = e => {
    const input = this.input;
    // Get the pressed key code
    var code = e.keyCode ? e.keyCode : e.which;

    if (
      code === 49 ||
      code === 50 ||
      code === 51 ||
      code === 52 ||
      code === 53
    ) {
      var pos = input.selectionStart;
      input.value = transformPinyin(input.value, pos, code - 48);
      input.selectionStart = pos;
      input.selectionEnd = pos;
      e.preventDefault();
      // Do stuff if it’s a space or one of the tone numbers (1-4)
    } else if (
      code === 49 ||
      code === 50 ||
      code === 51 ||
      code === 52 ||
      code === 53
    ) {
      // Get the value of the field
      let inputText = input.value + String.fromCharCode(code);

      // Run the replacer function for each numeric pīnyīn string match
      inputText = inputText.replace(/([a-zA-ZüÜ]+)([1-5])/g, pinyinReplace);

      // Update the text field value
      input.value = inputText;
      e.preventDefault();
    } else if (code === 58) {
      // ':'
      // Get the value of the field
      let inputText = input.value + String.fromCharCode(code);

      // Run the replacer function for each numeric pīnyīn string match
      inputText = inputText.replace(/u:/g, "ü");
      inputText = inputText.replace(/ū:/g, "ǖ");
      inputText = inputText.replace(/ú:/g, "ǘ");
      inputText = inputText.replace(/ǔ:/g, "ǚ");
      inputText = inputText.replace(/ù:/g, "ǜ");

      // Update the text field value
      input.value = inputText;
      e.preventDefault();
    } else if (code === 118) {
      // 'v'
      input.value += "ü";
      e.preventDefault();
    } else if (e.key === "Enter") {
      if (e.shiftKey) {
        this.props.onShiftEnter(input.value);
      } else {
        if (!this.props.onEnter(input.value)) {
          e.target.select();
        }
      }
      e.preventDefault();
    } else if (code === 32) {
      if (this.props.onSpace(input.value)) {
        e.preventDefault();
      }
    }
  };

  componentDidMount = () => {
    this.input.focus();
  };
  componentDidUpdate = (prevProps, prevState) => {
    this.input.focus();
  };

  handleRef = ref => {
    this.input = ref;
    if (this.props.ref) this.props.ref(ref);
  };

  render() {
    return (
      <input
        ref={this.handleRef}
        onKeyPress={this.handleKeyPress}
        onKeyDown={this.handleKeyDown}
        style={this.props.style}
        placeholder={this.props.placeholder}
        onFocus={this.props.onFocus}
        autoComplete="off"
        autoCorrect="off"
        autoCapitalize="off"
        spellCheck="false"
      />
    );
  }
}
