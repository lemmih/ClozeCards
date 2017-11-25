import $ from "jquery";

export default (editorA, editorB) => {
  if (!editorA || !editorB) return;
  const blocksA = $(editorA.refs.editor).find("[data-editor]");
  const blocksB = $(editorB.refs.editor).find("[data-editor]");
  const minBlocks = Math.min(blocksA.length, blocksB.length);
  const bottom =
    blocksA[minBlocks - 1].children[0].offsetHeight -
    blocksB[minBlocks - 1].children[0].offsetHeight;
  const dynamicProp = "min-height";
  const staticProp = "padding-bottom";

  {
    const topA = blocksA[0].offsetTop;
    const topB = blocksB[0].offsetTop;

    if (topA !== topB) {
      const addedA = getPadded(blocksA[0]);
      const addedB = getPadded(blocksB[0]);
      const deltaB = topA - addedA - (topB - addedB);
      const deltaA = topB - addedB - (topA - addedA);
      $(blocksB[0]).css("margin-top", Math.max(0, deltaB) + "px");
      $(blocksA[0]).css("margin-top", Math.max(0, deltaA) + "px");
      // console.log(0, topA, topB, addedA, addedB, deltaA, deltaB);
    }
  }

  for (var i = 1; i < minBlocks; i++) {
    const topA = blocksA[i].offsetTop;
    const topB = blocksB[i].offsetTop;
    const prevA = blocksA[i - 1];
    const prevB = blocksB[i - 1];
    // Size of the previous blocks without 'min-height'.
    const prevASize = prevA.children[0].offsetHeight;
    const prevBSize = prevB.children[0].offsetHeight;

    if (topB !== topA) {
      const addedA = getAdded(blocksA[i - 1]);
      const addedB = getAdded(blocksB[i - 1]);
      const deltaB = topA - addedA - (topB - addedB);
      const deltaA = topB - addedB - (topA - addedA);
      $(prevB).css(dynamicProp, Math.max(0, deltaB) + prevBSize + "px");
      $(prevA).css(dynamicProp, Math.max(0, deltaA) + prevASize + "px");
      // console.log(i, topA, topB, addedA, addedB, deltaA, deltaB);
    }
    $(blocksA[i]).css(staticProp, "0px");
    $(blocksB[i]).css(staticProp, "0px");
  }

  $(blocksA[minBlocks - 1]).css(staticProp, Math.max(0, -bottom) + "px");
  $(blocksB[minBlocks - 1]).css(staticProp, Math.max(0, bottom) + "px");

  for (let i = minBlocks; i < blocksA.length; i++) {
    $(blocksA[i]).css(dynamicProp, "0px");
    $(blocksA[i]).css(staticProp, "0px");
  }
  for (let i = minBlocks; i < blocksB.length; i++) {
    $(blocksB[i]).css(dynamicProp, "0px");
    $(blocksB[i]).css(staticProp, "0px");
  }
};

function getAdded(elt) {
  return Math.max(
    0,
    parseInt($(elt).css("min-height"), 10) - elt.children[0].offsetHeight
  );
}
function getPadded(elt) {
  return Math.max(0, parseInt($(elt).css("margin-top"), 10));
}
