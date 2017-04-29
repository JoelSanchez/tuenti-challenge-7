
var solve = function() {
    var $words = document.querySelectorAll('[id^="word-"]');
    var soup = getSoup();
    console.log('soup', soup, 'words', $words);
    $words.forEach(function(i) {
        solveWord(soup, i.innerHTML);
    });
};

var getSoup = function() {
    var len = document.querySelectorAll('table tr:first-child td').length;
    var soup = [];
    for (var i = 0; i < len; i++) {
        soup[i] = [];
        for (var j = 0; j < len; j++) {
            soup[i][j] = document.querySelector('[id="' + j + '-' + i + '"]').innerHTML;
        }
    }
    return soup;
};

var solveWord = function(soup, word) {
    var solution = wordSearch(soup, word);
    sendPosition(solution[0].x, solution[0].y, solution[solution.length - 1].x, solution[solution.length - 1].y);
};

var salt = function(t) {
    let n, e, o, s = 0;
    const c = t + "-saltbae";
    if (!c.length)
        return s;
    for (n = 0,
    o = c.length; n < o; n++)
        e = c.charCodeAt(n),
        s = (s << 5) - s + e,
        s |= 0;
    return Math.abs(s)
};

var sendPosition = function(x1, y1, x2, y2) {
    var f = x1 + '-' + y1;
    var t = x2 + '-' + y2;
    var e = f + '-' + t;
    w.send(btoa(e + '-' + salt(e)));
};

var search2D = function(soup, row, col, word) {

    if (soup[row][col] != word[0]) {
        return false;
    }

    var len = soup.length;

    var x = [ -1, -1, -1, 0, 0, 1, 1, 1 ];
    var y = [ -1, 0, 1, -1, 1, -1, 0, 1 ];
 
    for (var dir = 0; dir < 8; dir++) {
        var rd = row + x[dir];
        var cd = col + y[dir];

        for (var k = 1; k < word.length; k++) {
            if (rd >= len || rd < 0 || cd >= len || cd < 0) {
                break;
            }
 
            if (soup[rd][cd] != word[k]) {
                break;
            }
 
            rd += x[dir], cd += y[dir];
        }
 
        if (k == word.length) {
            var ret = [];
            var xPosition = col;
            var yPosition = row;
            for (var i = 0; i < word.length; i++) {
                ret.push({x: xPosition, y: yPosition});
                xPosition += y[dir];
                yPosition += x[dir];
            }
            return ret;
        }
    }

    return false;
}
 
var wordSearch = function(soup, word) {
    for (var row = 0; row < soup.length; row++) {
       for (var col = 0; col < soup.length; col++) {
            var result = search2D(soup, row, col, word);
            if (result) {
                return result;
            }
       }
    }
    return false;
}