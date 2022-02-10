let js = {|

const highlightLines = (renderedLines, noScroll=false) => {
  const mLines = location.hash.match(/L(\d+)(-L(\d+))*/)
  // Ignore non line-range hashes
  if (!mLines) {
    return
  }

  let [_, start, __, end] = mLines
  start = Number(start)
  end = end ? Number(end) : start
  // Return early if chosen lines are not rendered still
  if (renderedLines && start > renderedLines) {
    return
  }

  // Remove previous highlights if any
  document.querySelectorAll('.highlight').forEach(
    (node) => node.classList.remove('highlight')
  )
  // Highlight lines in selected range
  for (let i = start; i <= end; i++) {
    let lineSpan = document.getElementById('L' + String(i))
    if (!lineSpan) {
      break
    }
    lineSpan.classList.add('highlight')

    // Scroll to the first selected line, if not triggered by mutations
    if (i === start && !noScroll) {
      lineSpan.scrollIntoView()
    }
  }
}

const showLineNumbers = () => {
  const node = document.querySelector('pre')
  const oldLineNumbers = document.getElementById('line-numbers')
  const lineNumbers = oldLineNumbers ? oldLineNumbers : document.createElement('span')
  if (!oldLineNumbers) {
    lineNumbers.setAttribute('id', 'line-numbers')
    node.prepend(lineNumbers)
  }

  const lineCount = node.innerHTML.split(/\n/).length
  const oldLineCount = lineNumbers.getElementsByTagName('span').length
  for (let i = oldLineCount + 1; i <= lineCount; i++) {
    let number = document.createElement('span')
    number.innerHTML = i
    number.setAttribute('id', 'L' + String(i))
    lineNumbers.appendChild(number)
  }
  return lineCount
}

const addMutationObserver = () => {

  let debounceTimer
  const mutationCallback = function(mutationsList) {
    for(const mutation of mutationsList) {
      if (mutation.type === 'characterData') {
        clearTimeout(debounceTimer)
        debounceTimer = setTimeout(() => {
          const lineCount = showLineNumbers()
          highlightLines(lineCount, noScroll=true)
        }, 200)
      }
    }
  }

  // Create an observer instance and watch changes on the pre tag
  const observer = new MutationObserver(mutationCallback)
  const pre = document.querySelector('pre')
  observer.observe(pre, { subtree: true, characterData: true })
  console.debug('Added observer on pre tag')
}

// Wait for pre tag to load before attaching the mutation observer
const timer = setTimeout(addMutationObserver, 100)

document.addEventListener('DOMContentLoaded', (event) => {
  clearTimeout(timer) // If the page has loaded completely, don't add mutation observer
  showLineNumbers()
  highlightLines()
})
window.addEventListener('hashchange', () => {highlightLines()})

|}

let line_numbers = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get _ctx =
    let headers = Cohttp.Header.init_with "Content-Type" "text/javascript" in
    Utils.Server.respond_string ~status:`OK ~headers ~body:js ()
end
