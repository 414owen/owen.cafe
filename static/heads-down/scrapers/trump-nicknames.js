[...$$('tbody td[rowspan]:nth-child(2)')].forEach(a => {
    let row = a.parentNode;
    const rs = parseInt(a.getAttribute("rowspan"));
    console.log(rs);
    a.setAttribute("rowspan", 1);
    for (let i = 1; i < rs; i++) {
        row = row.nextSibling;
        row.appendChild(a.cloneNode(true));
    }
});

const cleanup = s => s.replace(/\[[0-9a-z]+\]/g, "").replace(/\n/g, "");

copy([...$$('tbody tr')].map(row => {
    const realPerson = row.querySelector('td:nth-child(2)');
    let res = cleanup(row.querySelector('td:first-child').innerText);
    if (realPerson) {
      res += ` (${cleanup(realPerson.innerText)})`;
    }
    return res;
}).join("\n"))
