/*jshint esversion: 6 */

const hashToLines = (renderedLines, noScroll = false) => {
  const mLines = location.hash.match(/L(\d+)(-L(\d+))*/);
  let start, end;
  if (mLines) {
    [_, start, __, end] = mLines;
    start = Number(start);
    end = end ? Number(end) : start;
    // Return early if chosen lines are not rendered still
    if (renderedLines && start > renderedLines) {
      return;
    }
  } else if (location.hash === "#end") {
    start = end = document.getElementById('line-numbers').getElementsByTagName('a').length;
    // Always scroll to the last line
    noScroll = false;
  } else {
    return;
  }
  highlightLines(start, end, !noScroll);
};

const highlightLines = (start, end, scroll) => {
  // Remove previous highlights if any
  document.querySelectorAll('.highlight').forEach(
    (node) => node.classList.remove('highlight')
  );
  // Highlight lines in selected range
  for (let i = start; i <= end; i++) {
    let line = document.getElementById('L' + String(i));
    if (!line) {
      break;
    }
    line.classList.add('highlight');
    // Scroll to the first selected line
    if (i === start && scroll) {
      line.scrollIntoView();
    }
  }
};

const showLineNumbers = () => {
  const node = document.querySelector('pre');
  const oldLineNumbers = document.getElementById('line-numbers');
  const lineNumbers = oldLineNumbers ? oldLineNumbers : document.createElement('span');
  if (!oldLineNumbers) {
    lineNumbers.setAttribute('id', 'line-numbers');
    node.prepend(lineNumbers);
  }

  const lineCount = node.innerHTML.split(/\n/).length;
  const oldLineCount = lineNumbers.getElementsByTagName('a').length;
  for (let i = oldLineCount + 1; i <= lineCount; i++) {
    let number = document.createElement('a');
    number.innerHTML = i;
    const id = 'L' + String(i);
    number.setAttribute('id', id);
    number.setAttribute('href', '#' + id);
    lineNumbers.appendChild(number);
  }
  return lineCount;
};

const addMutationObserver = () => {

  let debounceTimerFn = setTimeout(() => {
    const lineCount = showLineNumbers();
    highlightLines(lineCount, noScroll = true);
  }, 200);
  let debounceTimer;
  const mutationCallback = function (mutationsList) {
    for (const mutation of mutationsList) {
      if (mutation.type === 'characterData') {
        clearTimeout(debounceTimer);
        debounceTimer = debounceTimerFn();
      }
    }
  };

  // Create an observer instance and watch changes on the pre tag
  const observer = new MutationObserver(mutationCallback);
  const pre = document.querySelector('pre');
  observer.observe(pre, { subtree: true, characterData: true });
  console.debug('Added observer on pre tag');
};

// Wait for pre tag to load before attaching the mutation observer
const timer = setTimeout(addMutationObserver, 0.1);

document.addEventListener('DOMContentLoaded', (event) => {
    clearTimeout(timer); // If the page has loaded completely, don't add mutation observer
  showLineNumbers();
  hashToLines();
});
window.addEventListener('hashchange', () => { hashToLines(); });
