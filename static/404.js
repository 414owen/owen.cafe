(() => {
  const rand = max => Math.floor(Math.random() * max);
  const content = document.getElementById("404");
  let text = content.innerText.replace(/(^ +| +$)/, "");
  const len = text.length;
  const upperReg = /\p{Lu}/u;
  const isUpper = a => upperReg.test(a);
  const toggleCase = a => isUpper(a) ? a.toLowerCase() : a.toUpperCase();
  setInterval(() => {
    const ind = rand(len);
    text = text.slice(0, ind) + toggleCase(text[ind]) + text.slice(ind + 1);
    content.innerText = text;
  }, 100);
})();