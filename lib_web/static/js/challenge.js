// Proof-of-work browser challenge solver.
//
// Reads the challenge parameters from the data-* attributes of #challenge,
// finds a nonce whose SHA-256(challenge ":" nonce) has at least `difficulty`
// leading zero bits, then redirects to the verification endpoint to obtain a
// token cookie. HTTP-only crawlers never execute this and so never pass.
(async () => {
  const el = document.getElementById("challenge");
  const challenge = el.dataset.challenge;
  const difficulty = parseInt(el.dataset.difficulty, 10);
  const redirect = el.dataset.redirect;
  const verify = el.dataset.verify;
  const status = document.getElementById("status");
  const enc = new TextEncoder();

  function leadingZeroBits(bytes) {
    let z = 0;
    for (const b of bytes) {
      if (b === 0) { z += 8; continue; }
      z += Math.clz32(b) - 24;
      break;
    }
    return z;
  }

  let nonce = 0;
  for (;;) {
    const buf = await crypto.subtle.digest("SHA-256", enc.encode(challenge + ":" + nonce));
    if (leadingZeroBits(new Uint8Array(buf)) >= difficulty) break;
    nonce++;
    if (nonce % 2000 === 0) status.textContent = "Working… (" + nonce + ")";
  }

  location.href = verify
    + "?c=" + encodeURIComponent(challenge)
    + "&n=" + nonce
    + "&r=" + encodeURIComponent(redirect);
})();
