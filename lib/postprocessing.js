function join(sep,lst) {
    return lst.reduce(function(el,res){return el + sep+ res})
}

var tex = document.getElementsByTagName('eqns')
Array.prototype.forEach.call(tex, renderEqnarray);

function renderEqnarray(el) {
    var txt = el.innerText
    eqns = txt.split('\\\\')
    el.innerHTML = '<div class="eqnarray"><div class="eqns">'
	+join(' ',
	      eqns
	      .map(function(eq) {return '<eq>'+eq+'</eq><br/>'}))
	+'</div></div>'
}

tex = document.getElementsByTagName('eqn')
Array.prototype.forEach.call(tex, function(el) {
    katex.render('\\displaystyle '+el.innerText, el);
});

tex = document.getElementsByTagName('eq')
Array.prototype.forEach.call(tex, function(el) {
    katex.render(el.innerText, el);
});

var code = document.getElementsByTagName('cs')
Array.prototype.forEach.call(code, function(el) {
    el.innerHTML = "<code class='cs'>" + el.innerHTML + "</code>"
});

var code = document.getElementsByTagName('hs')
Array.prototype.forEach.call(code, function(el) {
    el.innerHTML = "<code class='hs'>" + el.innerHTML + "</code>"
});

var code = document.getElementsByTagName('l')
Array.prototype.forEach.call(code, function(el) {
    el.innerHTML = "<span class='promt'>*Main&gt;</span>"
});


var COURSE = "Функциональное и логическое программирование";
var N_MAX = 18;

header.innerHTML ='<table cellspacing="0" cellpadding="0"><tbody><tr style="height: 56px;"><td id="projectlogo"><img alt="Logo" src="img/Logo.jpg"/></td><td style="padding-left: 0.5em;">' + COURSE + '<hr><small>Методические указания к лабораторным работам</small></td></tr></tbody></table><hr><p class="navigation"> </p>';

var refs="<a href ='emacs-ref.html' target='_blank'>Справка по Emacs</a> | <a href ='haskell-ref.html'  target='_blank'>Справка по Haskell</a> | "

var index = "<a href='index.html' >Содержание</a> | ";


var fname = document.URL.match("flp-lab([0-9]+)[.]html");
if (fname)
{
    var n = Number(fname[1]);

    document.title = COURSE + ". Занятие №" + n;
    header.innerHTML += '<h1>Занятие №' + n + '</h1>';
    footer.innerHTML = '<hr><p class="navigation"> </p>';

    var prev = n == 1 ? "" : "<a href='flp-lab" + (n-1) + ".html' > << Предыдущая</a> | ";
    var next = n >= N_MAX ? "" : "<a href='flp-lab" + (n+1) + ".html' >Следующая >></a>";
    var p =document.getElementsByClassName("navigation");

    p[0].innerHTML = refs + prev + index  +  next;
    p[1].innerHTML = refs + prev + index  +  next;

}
else
{
    document.title = COURSE + ". Приложение";

    header.innerHTML += '<h1>Приложение</h1>';

    footer.innerHTML = '<hr><p class="navigation"> </p>';

    var p =document.getElementsByClassName("navigation");
    p[0].innerHTML = refs + index
    p[1].innerHTML = refs + index

}

var code = document.getElementsByTagName('h2')
Array.prototype.forEach.call(code, function(el) {
    var title = el.innerText
    el.innerHTML = "<a name='" + title  + "'>" + el.innerHTML + "</a>"
});
