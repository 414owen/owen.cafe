(function(){const t=document.createElement("link").relList;if(t&&t.supports&&t.supports("modulepreload"))return;for(const s of document.querySelectorAll('link[rel="modulepreload"]'))n(s);new MutationObserver(s=>{for(const r of s)if(r.type==="childList")for(const c of r.addedNodes)c.tagName==="LINK"&&c.rel==="modulepreload"&&n(c)}).observe(document,{childList:!0,subtree:!0});function o(s){const r={};return s.integrity&&(r.integrity=s.integrity),s.referrerPolicy&&(r.referrerPolicy=s.referrerPolicy),s.crossOrigin==="use-credentials"?r.credentials="include":s.crossOrigin==="anonymous"?r.credentials="omit":r.credentials="same-origin",r}function n(s){if(s.ep)return;s.ep=!0;const r=o(s);fetch(s.href,r)}})();const K=3,h=100,pe="http://www.w3.org/2000/svg",d=e=>document.getElementById(e),fe=27.5,b=d("boards"),_=d("start"),A=d("stop"),O=d("resume"),J=d("notes"),W=d("root-note-octave"),Y=d("root-note"),M=d("seed"),Q=d("random-seed"),Z=d("radius"),y=d("grid-size"),H=d("speed"),X=d("number-of-cells"),ee=d("subdivisions"),B=d("apply"),C=d("preset"),I={gridSize:y,key:Y,noteNumber:ee,numberOfCells:X,octave:W,radius:Z,scale:J,seed:M,speed:H},me=Object.keys,he=Object.values,ge=Object.entries,ye=(e,t)=>e.hasOwnProperty(t),P=me(I);let k;try{k=JSON.parse(atob(location.hash.replace(/^#/,"")))}catch{k={}}for(const e of P){const t=I[e];if(ye(k,e)){const o=k[e],n=t.value;o!==null&&(t.value=o,t.checkValidity()||(t.value=n))}else t.value||(t.value=t.getAttribute("value"))}const te=e=>()=>{const n=e*15485863;return e=n*n*n%2038074743/2038074743,e},x=(e,t)=>{for(let[o,n]of ge(t))e.setAttribute(o,n.toString())},be=(e,t,o={},n=[])=>{const s=document.createElementNS(e,t);x(s,o);for(let r of n)s.appendChild(r);return s},f=(e,t={},o=[])=>be(pe,e,t,o),p=Math.PI,V=p*2,ve=()=>{const e=f("circle",{x:0,y:0,r:K}),t=f("g",{class:"cell"},[e,f("path",{class:"cell",d:"M-2 0 L2 0 L1 -1 M2 0 L1 1"})]);return{x:0,y:0,radius:i.inputs.radius,direction:0,el:t,circleEl:e}},se=(e,t,o,n)=>{const{radius:s,gridSize:r}=i.inputs;t.x=s+Math.round(e()*r)*((o-s*2)/r),t.y=s+Math.round(e()*r)*((n-s*2)/r),t.direction=Math.floor(e()*4)*p/2+p/4},oe="begin",N="playing",ne="paused",we=(e,t,o)=>{const n=[];let s=0,r=0;for(;o--;){n.push(Math.pow(2,s/12)*e),r++,r%=t.length;const c=t[r];s+=c}return n},re=e=>{let t=fe;for(let o=0;o<Math.abs(e.octave);o++)e.octave>0?t*=2:t/=2;return t=Math.pow(2,e.key/12)*t,we(t,e.scale,e.noteNumber)},xe=(e,t,o,n={})=>f("rect",{id:"grid-rect",x:e,y:t,width:o,height:o,...n}),Ie=(e,t,o)=>{const n=i.inputs.gridSize,s={x1:0,x2:h,y1:0,y2:h,stroke:"#222","stroke-width":.3},r=f("pattern",{id:"grid",width:n,height:n,patternUnits:"userSpaceOnUse"},[f("line",s),f("line",s)]),c=xe(0,0,h,{fill:"url(#grid)"});return{el:f("svg",{class:"board",viewBox:`0 0 ${e} ${t}`,width:`${e*4}px`,height:`${t*4}px`},[f("defs",{},[r]),c,...o.map(l=>l.el)]),grid:r,gridRect:c,width:e,height:t,cells:o}},ce=()=>Array(i.inputs.numberOfCells).fill(0).map(()=>ve()),ie=()=>({gridSize:y.valueAsNumber,key:parseInt(Y.value),noteNumber:parseInt(ee.value),numberOfCells:X.valueAsNumber,octave:W.valueAsNumber,radius:Z.valueAsNumber,scale:J.value.split(/ +|,/).map(e=>parseFloat(e)),seed:parseFloat(M.value),speed:parseFloat(H.value)}),Ne=()=>{for(const e of he(I))if(!e.checkValidity())return!1;return!0},i=(()=>{const e=ie();return{state:oe,inputs:e,audioContext:new AudioContext,notes:re(e),boards:[]}})(),Se=(e,t)=>(Math.atan2(t,e)+V)%V,U=e=>{const{notes:t}=i;Ae(t[Math.min(t.length-1,Math.floor(e*t.length))])},S=(e,t,o)=>{const n=f("circle",{class:"bounce-anim",cx:t,cy:o});e.prepend(n),setTimeout(()=>{n.remove()},1e3)},Ce=e=>{const t=i.inputs.speed,{width:o,height:n,el:s}=e;for(const r of e.cells){let{x:c,y:a,radius:l,direction:u,circleEl:ue}=r,v=Math.cos(u)*t,w=Math.sin(u)*t;c+=v,a+=w;let L=!1,m=0;if(c-l<0&&u>=p/2&&u<=3*p/2||c+l>o&&(u<=p/2||u>=3*p/2)){v=-v;let g=v;m=(r.y-l)/(n-l*2),c-l<0?(g-=(c-l)*2,S(s,0,a)):(g-=c+l-o,S(s,o,a),m=1-m),c+=g,U(m),L=!0}if(a-l<0&&u>=p||a+l>n&&u<=p){w=-w;let g=w;m=(r.x-l)/(o-l*2),a-l<0?(g-=(a-l)*2,S(s,c,0)):(g-=a+l-n,S(s,c,n),m=1-m),a+=g,U(m),L=!0}L&&(u=Se(v,w),ue.style.fill=`hsl(${m*360}deg 100% 80%)`);const G=u/(p/4);if(Math.abs(G-Math.round(G))>.03)debugger;r.x=c,r.y=a,r.direction=u}},ke=()=>{for(const e of i.boards)Ce(e)},T=()=>{const t=i.inputs.radius/K;for(const o of i.boards)for(const n of o.cells){const{x:s,y:r,direction:c}=n;n.el.style.transform=`translate(${s}px, ${r}px) rotate(${c}rad) scale(${t})`}},z=()=>{i.state===N&&(ke(),T(),requestAnimationFrame(z))},Ae=e=>{const t=i.audioContext,o=t.currentTime,n=o,s=.3,r=o+s,c=t.createOscillator();c.type="sine",c.frequency.value=e;const a=t.createGain();a.connect(t.destination),a.gain.setValueAtTime(0,o),a.gain.linearRampToValueAtTime(.1,o+.06),c.connect(a),a.gain.linearRampToValueAtTime(0,r),c.start(n),c.stop(r),setTimeout(()=>{c.disconnect(),a.disconnect()},s*1010)},D=e=>e.style.visibility="visible",Le=e=>e.style.visibility="hidden",Ee=[_,A,O],R=()=>{for(const e of Ee)Le(e)},ae=()=>{b.classList.remove("grid-changed"),R(),D(A);const e=new AudioContext;i.audioContext=e,i.state=N,z()},le=()=>{R(),D(A),i.audioContext.resume(),i.state=N,z()},$=()=>{i.state===N&&(R(),D(O),i.state=ne,i.audioContext.suspend())},Oe=()=>{switch(i.state){case N:$();break;case ne:le();break;case oe:ae();break}};_.onclick=ae;O.onclick=le;A.onclick=$;addEventListener("keydown",e=>{switch(e.code){case"Space":e.preventDefault(),Oe();break}});let q=b,j=Date.now();const E=e=>{const t=Date.now(),o=e.target;(t>j+h||q!==o)&&(C.selectedIndex=0,q=e.target,j=t,B.disabled=!Ne())};for(const e of P){const t=I[e];t.addEventListener("input",E),t.addEventListener("change",E)}for(const e of[Q])e.addEventListener("click",E);Q.onclick=e=>{e.preventDefault();const t=Math.random();M.value=t.toString()};B.onclick=e=>{e.preventDefault(),de()};const de=()=>{B.disabled=!0,$();const e=ie();i.inputs=e,i.notes=re(e);const t=te(i.inputs.seed),o=btoa(JSON.stringify(e));location.hash=o;const n=document.querySelectorAll(".cell");for(const s of n)s.remove();for(const s of i.boards){const r=ce();s.cells=r;for(const c of r)se(t,c,s.width,s.height),s.el.appendChild(c.el)}T(),F()},F=()=>{for(const e of i.boards){const t=i.inputs.radius,n=(h-t*2)/i.inputs.gridSize,s=n/2,r=e.grid,c=e.gridRect;x(r,{width:n,height:n,x:t-s,y:t-s}),x(c,{x:0,y:0,width:h+t,height:h+t});{const a=r.children[0];x(a,{x1:s,x2:s,y1:0,y2:n})}{const a=r.children[1];x(a,{y1:s,y2:s,x1:0,x2:n})}}};y.addEventListener("mouseenter",()=>{b.classList.add("show-grid")});y.addEventListener("change",()=>{i.inputs.gridSize=parseInt(y.value),b.classList.add("grid-changed"),F()});y.addEventListener("mouseleave",()=>{b.classList.remove("show-grid")});C.oninput=()=>{if(C.value==="custom")return;const e=JSON.parse(C.value);for(const t of P)I[t].value=e[t];de()};i.boards.push(Ie(h,h,ce()));{const e=te(i.inputs.seed);for(const t of i.boards){b.appendChild(t.el);for(const o of t.cells)se(e,o,t.width,t.height)}}T();F();
