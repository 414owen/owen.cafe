---
title: "Lambda REPL"
date: 2026-03-14T10:09:57+02:00
layout: "blank"
summary: "Lambda calculus REPL"
---

<div style="float:right">
<b>Key:</b>

<ul class="lambda-listing">
<li class="param">param</li>
<li class="free">free variable</li>
<li class="bound">bound variable</li>
</ul>
</div>

<br>

<div class="tool-input-container">
  <textarea id="lambda-input" class="tool-input">(\a.aa)\ba.ca</textarea>
</div>

<br>

<div id="lambda-output"></div>

<script type="module">
import { parse, singleReduction, astToHtmlString } from "/lambda-calculus.js";

const input = document.getElementById("lambda-input");
const output = document.getElementById("lambda-output");

let timer = 0;

const debounce = (time, fn) => {
  let last = null;
  return (...args) => {
    if (last != null) clearTimeout(last);
    last = setTimeout(() => {
      fn(...args);
      last = null;
    }, time);
  };
};

input.oninput =
  debounce(300, () => {
    clearTimeout(timer);
    try {
      let ast = parse(input.value);
      const step = () => {
        const div = document.createElement("div");
        div.innerHTML = astToHtmlString(ast);
        output.appendChild(div);
        ast = singleReduction(ast)
        if (ast !== null) timer = setTimeout(step, 500);
      };
      output.innerHTML = "";
      step();
    } catch (err) {
      output.innerText = err.message;
    }
  });

input.oninput();

</script>
